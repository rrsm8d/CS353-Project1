#lang racket
(require racket/string)

(define players-hashmap (make-hash)) ; keeps track of a players total score
(define teams-hashmap (make-hash)) ; keeps track of a teams total score

; PRE: a frame (character)
; POST: T/F if it's a strike or not
(define (is-strike? char)
  (equal? char "X")
  )
; PRE: a frame (character)
; POST: T/F if it's a spare or not
(define (is-spare? char)
  (equal? char "/")
  )

; PRE: takes a list of strings
; POST: returns a list with every numeric string turned into an integer
(define (convert-to-number str)
  (if (string->number str)
      (string->number str) 
      str)
  )
; PRE: takes a list of lists of the player scores
; POST: returns a list where strings are converted to integers, if possible.
(define (str->int list)
  (map convert-to-number list)
  )

; PRE: takes an input filename
; POST: Returns a list of lists, containing each line, whitespace delimited.
(define (read-file filename)
  (let ([lines (file->lines filename)])
    (map str->int (map string-split lines)))
  )

; ***NOT NEEDED ANYMORE***
; PRE: any string
; POST: a list, after splitting the string by white spaces.
(define (split-line input-string)
  (string-split input-string)
  )

; PRE: takes an input filename
; POST: prepends the name of the team with each score entry. 
(define (prepend-team-name filename)
  (define (iter old-list new-list team)
    (cond
      [(empty? old-list) new-list] ; When finished, return new-list
      [(= (length (first old-list)) 1) (iter (rest old-list) new-list (first (first old-list)))] ; if line is team name, skip and set current team.
      [else (iter (rest old-list) (append new-list `((,team ,@(first old-list))) ) team)]) ; put team name before rest of list.
    )
  (iter (read-file filename) `() "Temp") ; Temp isn't used
  )

; ***NOT NEEDED ANYMORE***
; PRE: takes a list, this list being a game entry that was prepended with the team name
; POST: returns the team name
(define (what-team prepended-list)
  (car prepended-list)
  )

; ***NOT NEEDED ANYMORE***
; Pre: takes a list, this list being a game entry that was prepended with the team name
; Post: returns the team name
(define (game-frames prepended-list)
  (cdddr prepended-list)
  )

; ***NOT NEEDED ANYMORE***
; Partially generated with GPT-3.5. Prompt: "In racket, create a function that will iterate a list containing sublists. Do not perform anything on the sublists."
; PRE: takes a list
; POST: prints each item in the list.
(define (iterate-lists lst)
  (for-each (lambda (sublist)
              (for-each (lambda (item)
                          (display item)
                          (display " "))
                        sublist)
              (newline)
              )
            lst)
  )

; PRE: A list of frames
; POST: Calculates the score from that frame, return the score.
(define (calc-frames frame-list)
  (define (frame-value frame) ; What is the value of this specific frame?
    (cond
      [(number? frame) frame]
          [(is-strike? frame) 10]
          [(is-spare? frame) 10]
          [else 0])
    )
  (cond
    [(empty? frame-list) 0]
        [(= (length frame-list) 1) (frame-value (first frame-list))] ; Lists with one element.
        [(and (number? (first frame-list)) (number? (second frame-list))) (+ (frame-value (first frame-list)) (frame-value (second frame-list)))] ; A blow
        [(and (number? (first frame-list)) (is-spare? (second frame-list)) (= (length frame-list)2) 10)] ; A spare, but at the end of a list
        [(and (number? (first frame-list)) (is-spare? (second frame-list)) (= (length frame-list)3) (+ 10 (frame-value (third frame-list))))] ; A spare, at end of list AND with another frame at the end
        [(and (number? (first frame-list)) (is-spare? (second frame-list))) (+ 10 (frame-value (third frame-list)))] ; A spare
        [(and (is-strike? (first frame-list)) (>= (length frame-list) 3) (is-spare? (third frame-list))) (+ 10 10)] ; A strike followed by a spare
        [(and (is-strike? (first frame-list)) (>= (length frame-list) 3)) (+ 10 (frame-value (second frame-list)) (frame-value (third frame-list)))] ; A strike followed by numbers or more strikes
        [else 0])
  )

; PRE: A list of frames
; POST: Returns the total score for that round, passing each frame to calc-frames.
(define (score-round game-list)
  (define (iter frames total)
    (if (empty? frames) total
        (begin ; Want to remove 2 elements on blows and spares, except when the list is 1 item.
          (cond
            [(is-strike? (first frames)) (iter (rest frames) (+ total (calc-frames frames)))]
                [(and (= (length frames) 1) (number? (first frames))) (iter (rest frames) total)]
                [(= (length frames) 1) (iter (rest frames) (+ total (calc-frames frames)))]
                [else (iter (rest (rest frames)) (+ total (calc-frames frames)))]) ; DO NOT CHANGE THIS ***EVER*** AGAIN, IT WORKS
          ))
    )
  (iter game-list 0)
  )
  
  
; PRE: Takes a list of a round
; POST: Calculates the score, updates the hashmaps with said scores, optionally displays the scores made on each round.
(define (display-round-and-score round)
  (define (player-name round-list)
    (string-append (list-ref round-list 1) (list-ref round-list 2))
    )
  (define (player-score round-list)
    (score-round (cdddr round))
    )
  (define (update-hashmap)
    (define old-value (hash-ref players-hashmap (player-name round) 0))
    (hash-set! players-hashmap (player-name round) (+ (player-score round) old-value)) ; Update the score with the given increment
    (define old-value2 (hash-ref teams-hashmap (first round) 0)) ; TODO: Check if theres a way to avoid this repetition
    (hash-set! teams-hashmap (first round) (+ (player-score round) old-value2)) 
    )
   ; display the score for each round
  (display (first round))
  (display ": ")
  (display (player-name round))
  (display ": ")
  (display (player-score round))
  (display "\n")
  
  (update-hashmap)
  )

; Generated with GPT-3.5
; Prompt: "In racket, create a function that will neatly print a hashmap, showing its key and value on each line."
; PRE: a hashmap with values
; POST: prints all key value pairs in the hashmap
(define (print-hashmap hashmap)
  (for ([(key value) (in-hash hashmap)])
    (printf "~a: ~a\n" key value))
  )

; PRE: a hashmap with values
; POST: prints the maximum key/value pair from hash map
(define (print-max-value hashmap)
    (define max-pair #f)
    (for ([(key value) (in-hash hashmap)])
      (if (or (not max-pair) (> value (cdr max-pair)))
          (set! max-pair (cons key value)) 0))
    (when max-pair
      (printf "~a. Score: ~a\n" (car max-pair) (cdr max-pair)))
  )

; PRE: A list of lists, containing the rounds played
; POST: Displays the total scores made by each player and team.
(define (display-all filename)
  (define (score-each-round round-list)
    (cond
      [(empty? round-list) '()]
      [else (begin (display-round-and-score (first round-list))
                   (score-each-round (rest round-list)))])
    )
  (score-each-round (prepend-team-name filename))
  
  (display "\nScore from each player:\n")
  (print-hashmap players-hashmap)
  
  (display "\nScore for each team:\n")
  (print-hashmap teams-hashmap)
  
  (display "\nTop player:\n")
  (print-max-value players-hashmap)
  
  (display "\nWinning Team:\n")
  (print-max-value teams-hashmap)
  )

(display "NOTE: Scores do not reset after completion. Please run again to test with different input files.")
(display "\nRead a file with (display-all your-filename-here)")
;(display-all "scores.txt")


#| ; Testing code
(display (calc-frames `("X" 7 2 4 5 8 "/" 3 6 "X" "X" 5 "/" 9 "/" 1 8) ))
(display "\n\n")
(display (calc-frames `("X" "X" "X") ))
(display "\n\n")
(display (score-round `("X" 7 2 4 5 8 "/" 3 6 "X" "X" 5 "/" 9 "/" 1 8) ))
(display "\n\n")
(display (score-round `(7 "/" "X" 5 4 "X" "X" 7 "/" 5 4 8 "/" "X" 8 "/" "X")))
(display "\n\n")
|#