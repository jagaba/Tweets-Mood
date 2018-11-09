;AGABA JONAN
;2017/HD05/122U
;analying the moods of Tweets for CANADA (CA)
;#################################################################

#lang racket

(require net/url)
(require json)
(require data-science-master)
(require math)
(require plot)

;Defining API to retrieve tweets from. (CANADA)

(define getCountry (list (cons "UG" "0.3476, 32.5825")))
(define (getGeoCode country-code) 
    (define (retrieve-country fetched-code fetched-list)
        (cond ((null? fetched-list) "Undifined Country")
            (else (if
                    (eq? fetched-code (car (car fetched-list)))
                    (car fetched-list)
                    (retrieve-country fetched-code (cdr fetched-list))
                )
            )
        )
    )
    (retrieve-country country-code getCountry)
)

(define (create-boundary geoRadius) (string-append (cdr geoRadius) ",50mi"))
(define twitter-auth-token "AAAAAAAAAAAAAAAAAAAAAPVP3gAAAAAAxTYuOX91E6omOagmDF1jcsG5Vew%3DQUBDzRB3dOe7QwYXMMOEBRRChLDcB88jcvPqcKMFGFWHAWuMeE")

;Retrieving tweets Using an API

(define authHeader (string-append "Authorization: Bearer " twitter-auth-token))
(define TweetsGeocode (string-append "geocode" "=" (create-boundary (getGeoCode "UG"))))
(define TweetsCount (string-append "count" "=" "20"))
(define TweetsMode (string-append "tweet_mode" "=" "extended"))
(define apiUrl 
    (string-append 
    "https://api.twitter.com/1.1/search/tweets.json" "?" TweetsGeocode  "&" TweetsCount  "&" TweetsMode
    ) 
)
(define GoToUrl (string->url apiUrl))
(define twitterApiRequest (get-pure-port GoToUrl (list authHeader)))
(define resjson (port->string  twitterApiRequest))
(close-input-port twitterApiRequest)

;change response to data structure
(define resobj (string->jsexpr resjson))
(define Total-Tweets (hash-ref resobj 'statuses))


;Create String data from  Fetched Tweets
(define 
    (Create-String statuses)
    (let((bigstring ""))
        (define (fetch-result posts)
            (cond 
              ((null? posts) bigstring)
              (else
                (begin(set! bigstring (string-append bigstring (hash-ref (car posts) 'full_text)))
                    (fetch-result (cdr posts))
                )
              )
            )
        )
        (fetch-result Total-Tweets)
    )
)

(define Tweets-obtained (Create-String Total-Tweets))


;analyse mood

(display "Number of tweets analysed : ")
(display "[")
(display (length Total-Tweets))
(display "]")
(newline)

(display Tweets-obtained)

;document->tokens : conts number of words
;list->sentiment : assigns sentiment words
;aggregate : sum Occuracies of each

(define tokens (document->tokens Tweets-obtained #:sort? #t))
(define sentiment-data (list->sentiment tokens #:lexicon 'nrc))
(define sentiments-aggregate (aggregate sum ($ sentiment-data 'sentiment) ($ sentiment-data 'freq)))
(display sentiments-aggregate)

(newline)
(newline)
 
;;; Plotting the histgraph
(let ([counts (aggregate sum ($ sentiment-data 'sentiment) ($ sentiment-data 'freq))])
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (x y) (> (second x) (second y))))
	    #:color "Orange"
	    #:line-color "red"))
	  #:x-label "Affective Label"
	  #:y-label "Frequency")))
