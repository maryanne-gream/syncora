
;; title: event-registration
;; version:
;; summary:
;; description:

;; Event Registration Contract
;; Handles event creation, management, and ticket reservations

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-invalid-event (err u102))
(define-constant err-sold-out (err u103))
(define-constant err-already-reserved (err u104))
(define-constant err-not-organizer (err u105))
(define-constant err-expired (err u106))
(define-constant err-invalid-price (err u107))

;; Define event status options
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-CANCELLED u2)
(define-constant STATUS-COMPLETED u3)

;; Data Types
(define-map events 
    { event-id: uint }
    {
        organizer: principal,
        name: (string-utf8 100),
        description: (string-utf8 500),
        date: uint,            ;; Unix timestamp
        venue: (string-utf8 100),
        ticket-price: uint,    ;; In STX
        max-capacity: uint,
        tickets-sold: uint,
        status: uint,
        image-url: (string-utf8 200)
    })

(define-map event-tickets
    { event-id: uint, ticket-id: uint }
    {
        owner: principal,
        purchase-date: uint,
        status: uint,         ;; 1: reserved, 2: purchased, 3: used, 4: cancelled
        payment-tx: (optional (string-ascii 64))
    })

(define-map user-tickets
    { user: principal }
    (list 20 {event-id: uint, ticket-id: uint}))

(define-map organizer-events
    { organizer: principal }
    (list 20 uint))

;; Data Variables
(define-data-var next-event-id uint u1)
(define-data-var next-ticket-id uint u1)

;; Administrative Functions
(define-public (register-event 
        (name (string-utf8 100))
        (description (string-utf8 500))
        (date uint)
        (venue (string-utf8 100))
        (ticket-price uint)
        (max-capacity uint)
        (image-url (string-utf8 200)))
    (let ((event-id (var-get next-event-id)))
        ;; Validate inputs
        (asserts! (> date block-height) err-invalid-event)
        (asserts! (> ticket-price u0) err-invalid-price)
        (asserts! (> max-capacity u0) err-invalid-event)
        
        ;; Create event
        (map-set events 
            { event-id: event-id }
            {
                organizer: tx-sender,
                name: name,
                description: description,
                date: date,
                venue: venue,
                ticket-price: ticket-price,
                max-capacity: max-capacity,
                tickets-sold: u0,
                status: STATUS-ACTIVE,
                image-url: image-url
            })
        
        ;; Update organizer's event list
        (match (map-get? organizer-events {organizer: tx-sender})
            prev-events (map-set organizer-events 
                {organizer: tx-sender}
                (unwrap-panic (as-max-len? (append prev-events event-id) u20)))
            (map-set organizer-events 
                {organizer: tx-sender}
                (list event-id)))
        
        ;; Increment event counter
        (var-set next-event-id (+ event-id u1))
        (ok event-id)))

;; Ticket Management Functions
(define-public (reserve-ticket (event-id uint))
    (let ((event (unwrap! (map-get? events {event-id: event-id}) err-not-found))
         (ticket-id (var-get next-ticket-id)))
        
        ;; Validate event status
        (asserts! (is-eq (get status event) STATUS-ACTIVE) err-invalid-event)
        (asserts! (< (get tickets-sold event) (get max-capacity event)) err-sold-out)
        (asserts! (> (get date event) block-height) err-expired)
        
        ;; Create ticket reservation
        (map-set event-tickets
            {event-id: event-id, ticket-id: ticket-id}
            {
                owner: tx-sender,
                purchase-date: block-height,
                status: u1,  ;; Reserved
                payment-tx: none
            })
        
        ;; Update user's ticket list
        (match (map-get? user-tickets {user: tx-sender})
            prev-tickets (map-set user-tickets
                {user: tx-sender}
                (unwrap-panic (as-max-len? 
                    (append prev-tickets {event-id: event-id, ticket-id: ticket-id}) 
                    u20)))
            (map-set user-tickets
                {user: tx-sender}
                (list {event-id: event-id, ticket-id: ticket-id})))
        
        ;; Increment counters
        (map-set events 
            {event-id: event-id}
            (merge event {tickets-sold: (+ (get tickets-sold event) u1)}))
        (var-set next-ticket-id (+ ticket-id u1))
        (ok ticket-id)))

(define-public (purchase-reserved-ticket (event-id uint) (ticket-id uint))
    (let ((event (unwrap! (map-get? events {event-id: event-id}) err-not-found))
         (ticket (unwrap! (map-get? event-tickets {event-id: event-id, ticket-id: ticket-id}) err-not-found)))
        
        ;; Validate ticket status
        (asserts! (is-eq (get owner ticket) tx-sender) err-owner-only)
        (asserts! (is-eq (get status ticket) u1) err-already-reserved)
        
        ;; Process payment
        (try! (stx-transfer? (get ticket-price event) tx-sender (get organizer event)))
        
        ;; Update ticket status
        (ok (map-set event-tickets
            {event-id: event-id, ticket-id: ticket-id}
            (merge ticket {
                status: u2,
            }))
        )
    )
)

;; Event Management Functions
(define-public (update-event-status (event-id uint) (new-status uint))
    (let ((event (unwrap! (map-get? events {event-id: event-id}) err-not-found)))
        (asserts! (is-eq (get organizer event) tx-sender) err-not-organizer)
        (asserts! (<= new-status u3) err-invalid-event)
        
        (map-set events
            {event-id: event-id}
            (merge event {status: new-status}))
        (ok true)))

;; Getter Functions
(define-read-only (get-event (event-id uint))
    (map-get? events {event-id: event-id}))

(define-read-only (get-ticket (event-id uint) (ticket-id uint))
    (map-get? event-tickets {event-id: event-id, ticket-id: ticket-id}))

(define-read-only (get-user-tickets (user principal))
    (map-get? user-tickets {user: user}))

(define-read-only (get-organizer-events (organizer principal))
    (map-get? organizer-events {organizer: organizer}))

(define-read-only (get-event-tickets (event-id uint))
    (let ((event (unwrap! (map-get? events {event-id: event-id}) err-not-found)))
        (ok {
            sold: (get tickets-sold event),
            available: (- (get max-capacity event) (get tickets-sold event)),
            price: (get ticket-price event)
        })))