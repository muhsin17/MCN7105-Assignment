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

;;; Classify tweets
;;ideally here the country is already predefined...Improvement would be to have it as a query that is entered by the user
(define classified-tweets
  (map (λ (entry) (list (first entry)
                        (if (string-contains? (second entry) "Belgium") "belgium" "other")))
       clean-tweets))

;;; Filter tweets
(define bel-tweets (filter (λ (entry) (string=? (second entry) "belgium")) classified-tweets))

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
(define processed-bel-tweets (tokenize-remove-stopwords ($ bel-tweets 0)))

;;; Analyze sentiment using the NRC lexicon
(define (perform-sentiment-analysis tokenized-tweets)
  (list->sentiment tokenized-tweets #:lexicon 'nrc))

;;; Perform sentiment analysis for tweets
(define bel-sentiment-data (perform-sentiment-analysis (append* processed-bel-tweets)))

;;; Aggregate sentiment counts for plotting
(define (calculate-sentiment-frequencies sentiment-data)
  (aggregate sum ($ sentiment-data 'sentiment) ($ sentiment-data 'freq)))

(define bel-sentiment-frequencies (calculate-sentiment-frequencies bel-sentiment-data))

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

;;; Plot sentiment analysis for tweets
(plot-sentiment-data bel-sentiment-frequencies "Belgium")
