(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (if (zero? n)
                   acc
                   (recur (* base acc) (dec n))))]
    (if (zero? exp)
      1
      (helper base (dec exp)))))

(defn last-element [a-seq]
  (let [helper (fn [acc a-seq]
                 (if (empty? a-seq)
                   acc
                   (recur (first a-seq) (rest a-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [s1 s2]
                 (cond
                  (and (empty? s1) (empty? s2)) true
                  (or (empty? s1) (empty? s2)) false
                  (not (== (first s1) (first s2))) false
                  :else (recur (rest s1) (rest s2))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (let [helper (fn [s n idx]
                 (if (empty? s)
                   idx
                   (if (and (pred (first s)) (nil? idx))
                     (recur (rest s) (inc n) n)
                     (recur (rest s) (inc n) idx))))]
    (helper a-seq 0 nil)))

(defn avg [a-seq]
  (let [helper (fn [sq n sum]
                 (if (empty? sq)
                   (/ sum n)
                   (recur (rest sq) (inc n) (+ sum (first sq)))))]
    (helper a-seq 0 0)))

(defn parity [a-seq]
  (let [helper (fn [d-seq acc]
                 (if (empty? d-seq)
                   acc
                   (if (odd? (count (filter (fn [x] (= x (first d-seq))) a-seq)))
                     (recur (rest d-seq) (cons (first d-seq) acc))
                     (recur (rest d-seq) acc))))]
    (helper (distinct a-seq) '())))

(defn fast-fibo [n]
  (loop [cur-n 0 sum 0 fn-1 0 fn-2 0]
    (cond
     (> cur-n n) sum
     (= cur-n 0) (recur (inc cur-n) 0 0 0)
     (= cur-n 1) (recur (inc cur-n) 1 0 1)
     (= cur-n 2) (recur (inc cur-n) 1 1 1)
     :else (recur (inc cur-n) (+ fn-1 fn-2) (+ fn-1 fn-2) fn-1))))

(defn cut-at-repetition [a-seq]
  (loop [sq a-seq collected []]
    (cond
     (empty? sq) collected
     (some (fn [x] (= x (first sq))) collected) collected
     :else (recur (rest sq) (conj collected (first sq))))))
