#lang racket

(require data-science-master)
(require plot)
(require math)
(require json)

;;; Function to convert line-oriented JSON to an array
(define (convert-json-lines-to-array #:limit [limit #f])
  (let loop ([count 0]
             [json-array '()]
             [record (read-json (current-input-port))])
    (if (or (eof-object? record)
            (and limit (>= count limit)))
        (jsexpr->string json-array)
        (loop (add1 count)
              (cons record json-array)
              (read-json (current-input-port))))))

;;; Read and parse the tweet database
(define tweets (string->jsexpr
                         (with-input-from-file "sample_tweets.json"
                           (λ () (convert-json-lines-to-array)))))

;;; Clean and process tweet data
(define clean-tweets
  (let ([temp-data (map (λ (entry) (list (hash-ref entry 'text)
                                         (hash-ref entry 'partition_1)
                                         (hash-ref entry 'created_at)))
                        (car tweets))])
    (filter (λ (entry) (not (string-prefix? (first entry) "RT"))) temp-data)))

;;; Classify tweets based on user input location
(define (classify-tweets-by-location location)
  (map (λ (entry) (list (first entry)
                        (if (string-contains? (second entry) location)
                            location
                            "other")))
       clean-tweets))

;;; Filter tweets based on user input location
(define (filter-tweets-by-location tweets location)
  (filter (λ (entry) (string=? (second entry) location)) tweets))

;;; Function to preprocess tweet text
(define (preprocess-text tweet-text)
  (string-normalize-spaces
   (remove-punctuation
    (remove-urls
     (string-downcase tweet-text)) #:websafe? #t)))

;;; Tokenize and remove stopwords
(define (tokenize-remove-stopwords tweet-collection)
  (map (λ (tweet)
         (remove-stopwords
          (document->tokens (preprocess-text tweet) #:sort? #t)))
       tweet-collection))

;;; Tokenize and clean tweets
(define (process-tweets-for-location location)
  (let* ([classified-tweets (classify-tweets-by-location location)])
    (if (not (member "other" (map second classified-tweets)))
        (tokenize-remove-stopwords ($ (filter-tweets-by-location classified-tweets location) 0))
        '())))

;;; Analyze sentiment using the NRC lexicon
(define (perform-sentiment-analysis tokenized-tweets)
  (list->sentiment tokenized-tweets #:lexicon 'nrc))

;;; Aggregate sentiment counts for plotting
(define (calculate-sentiment-frequencies sentiment-data)
  (aggregate sum ($ sentiment-data 'sentiment) ($ sentiment-data 'freq)))

;;; Plot sentiment analysis results
(define (plot-sentiment-data sentiment-frequencies country-name)
  (parameterize ((plot-width 800))
    (plot (list
           (tick-grid)
           (discrete-histogram
            (sort sentiment-frequencies (λ (x y) (> (second x) (second y))))
            #:color "green"
            #:line-color "MediumSlateBlue"))
          #:x-label "Emotions"
          #:y-label "Frequency"
          #:title (string-append "Sentiment Analysis for " country-name " Tweets"))))

;;; Main function for user input and processing
(define (main)
  (display "Enter the location (e.g., 'Belgium'): ")
  (define location (read-line))
  (define processed-tweets (process-tweets-for-location location))
  (if (null? processed-tweets)
      (display "No tweets found for the specified location.\n")
      (let* ([sentiment-data (perform-sentiment-analysis (append* processed-tweets))]
            [sentiment-frequencies (calculate-sentiment-frequencies sentiment-data)])
        (plot-sentiment-data sentiment-frequencies location))))

;;; Run the program
(main)
