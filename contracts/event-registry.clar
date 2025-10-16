;; Event Registry Contract for Intic.id
;; Central registry to track all deployed event NFT contracts
;; Version: 1.0.0

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-UNAUTHORIZED (err u101))
(define-constant ERR-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-EXISTS (err u103))
(define-constant ERR-INVALID-INPUT (err u104))

;; Data Variables
(define-data-var next-event-id uint u1)
(define-data-var platform-fee-percentage uint u250) ;; 2.5% platform fee

;; Event Registry Map
(define-map events uint {
  event-id: uint,
  contract-address: principal,
  contract-name: (string-ascii 128),
  event-name: (string-utf8 256),
  organizer: principal,
  category: (string-utf8 64),
  deployed-at: uint,
  event-date: uint,
  total-supply: uint,
  ticket-price: uint,
  currency: (string-ascii 10),
  metadata-uri: (string-ascii 256),
  is-active: bool,
  is-verified: bool
})

;; Organizer Events - Track events by organizer
(define-map organizer-events principal (list 100 uint))

;; Event Statistics
(define-map event-stats uint {
  tickets-sold: uint,
  total-revenue: uint,
  last-updated: uint
})

;; Category Index - For filtering
(define-map category-events (string-utf8 64) (list 100 uint))

;; Verified Organizers
(define-map verified-organizers principal bool)

;; Platform Statistics
(define-map platform-stats (string-ascii 32) uint)

;; Register new event contract
(define-public (register-event
  (contract-address principal)
  (contract-name (string-ascii 128))
  (event-name (string-utf8 256))
  (category (string-utf8 64))
  (event-date uint)
  (total-supply uint)
  (ticket-price uint)
  (currency (string-ascii 10))
  (metadata-uri (string-ascii 256)))
  (let
    (
      (event-id (var-get next-event-id))
      (current-events (default-to (list) (map-get? organizer-events tx-sender)))
      (category-events-list (default-to (list) (map-get? category-events category)))
    )

    ;; Validations
    (asserts! (> (len event-name) u0) ERR-INVALID-INPUT)
    (asserts! (> total-supply u0) ERR-INVALID-INPUT)
    (asserts! (> ticket-price u0) ERR-INVALID-INPUT)

    ;; Register event
    (map-set events event-id {
      event-id: event-id,
      contract-address: contract-address,
      contract-name: contract-name,
      event-name: event-name,
      organizer: tx-sender,
      category: category,
      deployed-at: block-height,
      event-date: event-date,
      total-supply: total-supply,
      ticket-price: ticket-price,
      currency: currency,
      metadata-uri: metadata-uri,
      is-active: true,
      is-verified: false
    })

    ;; Initialize stats
    (map-set event-stats event-id {
      tickets-sold: u0,
      total-revenue: u0,
      last-updated: block-height
    })

    ;; Add to organizer's events
    (map-set organizer-events tx-sender
      (unwrap! (as-max-len? (append current-events event-id) u100) ERR-INVALID-INPUT))

    ;; Add to category index
    (map-set category-events category
      (unwrap! (as-max-len? (append category-events-list event-id) u100) ERR-INVALID-INPUT))

    ;; Increment event counter
    (var-set next-event-id (+ event-id u1))

    ;; Update platform stats
    (update-platform-stat "total-events" u1)

    (ok event-id)))

;; Update event status (organizer only)
(define-public (update-event-status (event-id uint) (is-active bool))
  (let ((event (unwrap! (map-get? events event-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender (get organizer event)) ERR-UNAUTHORIZED)
    (map-set events event-id (merge event { is-active: is-active }))
    (ok true)))

;; Update event statistics (called by event contracts or indexer)
(define-public (update-event-stats
  (event-id uint)
  (tickets-sold uint)
  (total-revenue uint))
  (let ((event (unwrap! (map-get? events event-id) ERR-NOT-FOUND)))
    ;; Only contract owner or event organizer can update stats
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER)
                  (is-eq tx-sender (get organizer event)))
              ERR-UNAUTHORIZED)

    (map-set event-stats event-id {
      tickets-sold: tickets-sold,
      total-revenue: total-revenue,
      last-updated: block-height
    })

    (ok true)))

;; Verify organizer (platform admin only)
(define-public (verify-organizer (organizer principal) (verified bool))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (map-set verified-organizers organizer verified)
    (ok true)))

;; Verify event (platform admin only)
(define-public (verify-event (event-id uint))
  (let ((event (unwrap! (map-get? events event-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (map-set events event-id (merge event { is-verified: true }))
    (ok true)))

;; Read-only functions

(define-read-only (get-event (event-id uint))
  (map-get? events event-id))

(define-read-only (get-event-stats (event-id uint))
  (map-get? event-stats event-id))

(define-read-only (get-organizer-events (organizer principal))
  (ok (default-to (list) (map-get? organizer-events organizer))))

(define-read-only (get-category-events (category (string-utf8 64)))
  (ok (default-to (list) (map-get? category-events category))))

(define-read-only (is-organizer-verified (organizer principal))
  (default-to false (map-get? verified-organizers organizer)))

(define-read-only (get-total-events)
  (ok (- (var-get next-event-id) u1)))

(define-read-only (get-platform-fee)
  (ok (var-get platform-fee-percentage)))

;; Get event with full details (event + stats)
(define-read-only (get-event-details (event-id uint))
  (let
    (
      (event-data (map-get? events event-id))
      (stats-data (map-get? event-stats event-id))
    )
    (ok {
      event: event-data,
      stats: stats-data
    })))

;; Get multiple events (for listing/pagination)
(define-read-only (get-events-range (start-id uint) (count uint))
  (ok (map get-event-if-exists
    (list start-id
          (+ start-id u1)
          (+ start-id u2)
          (+ start-id u3)
          (+ start-id u4)))))

(define-read-only (get-event-if-exists (event-id uint))
  (map-get? events event-id))

;; Helper function to update platform stats
(define-private (update-platform-stat (stat-name (string-ascii 32)) (increment uint))
  (let ((current-value (default-to u0 (map-get? platform-stats stat-name))))
    (map-set platform-stats stat-name (+ current-value increment))
    true))

;; Get platform stats
(define-read-only (get-platform-stats)
  (ok {
    total-events: (default-to u0 (map-get? platform-stats "total-events")),
    total-organizers: (default-to u0 (map-get? platform-stats "total-organizers"))
  }))

;; Admin functions

(define-public (set-platform-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (asserts! (<= new-fee u1000) ERR-INVALID-INPUT) ;; Max 10%
    (var-set platform-fee-percentage new-fee)
    (ok true)))

(define-public (remove-event (event-id uint))
  (let ((event (unwrap! (map-get? events event-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (map-set events event-id (merge event { is-active: false }))
    (ok true)))
