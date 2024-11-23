
;; title: event-refund
;; version:
;; summary:
;; description:

;; Event Refund Contract
;; Handles refund policies, refund requests, and refund processing

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-policy (err u103))
(define-constant err-not-eligible (err u104))
(define-constant err-already-refunded (err u105))
(define-constant err-expired (err u106))
(define-constant err-event-active (err u107))
(define-constant err-invalid-amount (err u108))

;; Refund Policy Types
(define-constant REFUND-POLICY-FULL u1)        ;; Full refund available
(define-constant REFUND-POLICY-PARTIAL u2)     ;; Partial refund with percentage
(define-constant REFUND-POLICY-TIME-BASED u3)  ;; Refund based on time until event
(define-constant REFUND-POLICY-NONE u4)        ;; No refunds allowed

;; Refund Request Status
(define-constant STATUS-PENDING u1)
(define-constant STATUS-APPROVED u2)
(define-constant STATUS-REJECTED u3)
(define-constant STATUS-PROCESSED u4)

;; Data Maps
(define-map refund-policies
    { event-id: uint }
    {
        policy-type: uint,
        refund-percentage: uint,     ;; For partial refunds (0-100)
        deadline: uint,              ;; Timestamp until when refunds are allowed
        min-days-before: uint,       ;; Minimum days before event for refund
        cancellation-policy: bool    ;; Whether full refund on event cancellation
    })

(define-map refund-requests
    { ticket-id: uint }
    {
        event-id: uint,
        requester: principal,
        request-time: uint,
        reason: (string-utf8 200),
        status: uint,
        amount: uint,
        processed-time: (optional uint),
        processor: (optional principal)
    })

(define-map processed-refunds
    { ticket-id: uint }
    {
        amount: uint,
        process-time: uint,
        tx-id: (optional (string-ascii 64))
    })

;; Event organizers can set refund policy
(define-public (set-refund-policy
        (event-id uint)
        (policy-type uint)
        (refund-percentage uint)
        (deadline uint)
        (min-days-before uint)
        (cancellation-policy bool))
    (let 
      (
        (event (unwrap! (contract-call? .event-registration get-event event-id) err-not-found))
      )
        ;; Verify caller is event organizer
        (asserts! (is-eq tx-sender (get organizer event)) err-unauthorized)
        ;; Validate policy parameters
        (asserts! (<= policy-type u4) err-invalid-policy)
        (asserts! (<= refund-percentage u100) err-invalid-policy)
        (asserts! (>= deadline block-height) err-invalid-policy)
        
        ;; Set the policy
        (map-set refund-policies
            { event-id: event-id }
            {
                policy-type: policy-type,
                refund-percentage: refund-percentage,
                deadline: deadline,
                min-days-before: min-days-before,
                cancellation-policy: cancellation-policy
            })
        (ok true)))

;; Users can request refunds
(define-public (request-refund (ticket-id uint) (reason (string-utf8 200)))
    (let (
        (ticket (unwrap! (contract-call? .event-registration get-ticket event-id ticket-id) err-not-found))
        (event-id (get event-id ticket))
        (policy (unwrap! (map-get? refund-policies {event-id: event-id}) err-not-found))
        (event (unwrap! (contract-call? .event-registration get-event event-id) err-not-found)))
        
        ;; Verify ticket ownership
        (asserts! (is-eq tx-sender (get owner ticket)) err-unauthorized)
        ;; Check if ticket already refunded
        (asserts! (is-none (map-get? processed-refunds {ticket-id: ticket-id})) err-already-refunded)
        
        ;; Check if event is still active
        (asserts! (not (is-eq (get status event) REFUND-POLICY-NONE)) err-not-eligible)
        
        ;; Calculate refund amount based on policy
        (let ((refund-amount (calculate-refund-amount event policy)))
            ;; Verify refund amount is valid
            (asserts! (> refund-amount u0) err-not-eligible)
            
            ;; Create refund request
            (map-set refund-requests
                { ticket-id: ticket-id }
                {
                    event-id: event-id,
                    requester: tx-sender,
                    request-time: block-height,
                    reason: reason,
                    status: STATUS-PENDING,
                    amount: refund-amount,
                    processed-time: none,
                    processor: none
                })
            (ok refund-amount))))

;; Get specific ticket details
(define-read-only (get-ticket-details (ticket-id uint))
    (let ((ticket (unwrap! (contract-call? .event-registration get-ticket ticket-id) err-not-found)))
        (ok {
            owner: (get owner ticket),
            event-id: (get event-id ticket),
            status: (get status ticket),
            purchase-date: (get purchase-date ticket)
        })))

;; Calculate refund amount based on policy
(define-private (calculate-refund-amount (event {organizer: principal, ticket-price: uint, date: uint, status: uint}) 
                                       (policy {policy-type: uint, refund-percentage: uint, deadline: uint, min-days-before: uint}))
    (let ((days-until-event (/ (- (get date event) block-height) u144)))  ;; Assuming ~144 blocks per day
        (if (is-eq (get status event) u2)  ;; If event is cancelled
            (get ticket-price event)  ;; Full refund
            (if (is-eq (get policy-type policy) REFUND-POLICY-FULL)
                (get ticket-price event)
                (if (is-eq (get policy-type policy) REFUND-POLICY-PARTIAL)
                    (/ (* (get ticket-price event) (get refund-percentage policy)) u100)
                    (if (is-eq (get policy-type policy) REFUND-POLICY-TIME-BASED)
                        (if (>= days-until-event (get min-days-before policy))
                            (get ticket-price event)
                            u0)
                        u0))))))  ;; Default case (REFUND-POLICY-NONE)



;; Process refund
(define-public (process-refund (ticket-id uint))
    (let ((request (unwrap! (map-get? refund-requests {ticket-id: ticket-id}) err-not-found))
         (event (unwrap! (contract-call? .event-registration get-event (get ticket-id request)) err-not-found)))
        
        ;; Verify caller is event organizer
        (asserts! (is-eq tx-sender (get organizer event)) err-unauthorized)
        ;; Verify request is pending
        (asserts! (is-eq (get status request) STATUS-PENDING) err-already-refunded)
        
        ;; Process payment
        (try! (stx-transfer? (get amount request) tx-sender (get requester request)))
        
        ;; Update request status
        (map-set refund-requests
            { ticket-id: ticket-id }
            (merge request {
                status: STATUS-PROCESSED,
                processed-time: (some block-height),
                processor: (some tx-sender)
            }))
        
        ;; Record processed refund
        (map-set processed-refunds
            { ticket-id: ticket-id }
            {
                amount: (get amount request),
                process-time: block-height,
            })
        (ok true)))

;; Bulk process refunds for cancelled event
(define-public (process-event-refunds (event-id uint))
    (let ((event (unwrap! (contract-call? .event-registration get-event event-id) err-not-found)))
        ;; Verify caller is event organizer
        (asserts! (is-eq tx-sender (get organizer event)) err-unauthorized)
        ;; Verify event is cancelled
        (asserts! (is-eq (get status event) u2) err-event-active)
        
        ;; Process refunds for all tickets (implementation would need to handle pagination)
        (ok true)))

;; Getter Functions
(define-read-only (get-refund-policy (event-id uint))
    (map-get? refund-policies {event-id: event-id}))

(define-read-only (get-refund-request (ticket-id uint))
    (map-get? refund-requests {ticket-id: ticket-id}))

(define-read-only (get-processed-refund (ticket-id uint))
    (map-get? processed-refunds {ticket-id: ticket-id}))

(define-public (check-refund-eligibility (ticket-id uint))
    (let ((ticket (unwrap! (contract-call? .event-registration get-ticket-info ticket-id) err-not-found))
         (event-id (get event-id ticket))
         (policy (unwrap! (map-get? refund-policies {event-id: event-id}) err-not-found))
         (event (unwrap! (contract-call? .event-registration-contract get-event event-id) err-not-found)))
        
        (ok {
            eligible: (and 
                (>= (get deadline policy) block-height)
                (is-none (map-get? processed-refunds {ticket-id: ticket-id}))
            ),
            potential-amount: (calculate-refund-amount event policy)
        })))