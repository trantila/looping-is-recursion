(ns looping-is-recursion)

(defn power [base exp]
  (let [impl (fn [current exp]
               (if (<= exp 0)
                  current
                  (recur (* current base) (dec exp))))]
    (impl 1 exp)))

(defn last-element [a-seq]
  (let [impl (fn [current, a-seq]
               (if (empty? a-seq)
                 current
                 (recur (first a-seq) (rest a-seq))))]
    (impl (first a-seq) a-seq)))

(defn seq= [seq1 seq2]
  ; C/P from previous (seq= -> recur).
  (cond (and (empty? seq1) (empty? seq2)) true                        ; all were equal
        (or (empty? seq1) (empty? seq2)) false                        ; only the other ran out
        (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2)) ; current elements equal
        :else false))                                                 ; current elements not equal

(defn find-first-index [pred a-seq]
  (loop [current-index 0
         a-seq a-seq]
    (cond
      (empty? a-seq) nil
      (pred (first a-seq)) current-index
      :else (recur (inc current-index) (rest a-seq)))))

(defn avg [a-seq]
  (loop [n 0
         v 0
         a-seq a-seq]
    (if (empty? a-seq)
      (/ v n)
      (recur (inc n) (+ v (first a-seq)) (rest a-seq)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))]
    (loop [parities #{}
           a-seq a-seq]
      (if (empty? a-seq)
        parities
        (recur (toggle parities (first a-seq)) (rest a-seq))))))

(defn fast-fibo "Less horrible fibonacci generator without compiler magic" [n]
  (loop [previous 0
         current 1
         index 0]
    (if (>= index n)
      previous
      (recur current (+ previous current) (inc index)))))

(defn cut-at-repetition [a-seq]
  (loop [seen-values #{}
         seq-in a-seq
         seq-out []]
    (let [current (first seq-in)]
      (cond
        (empty? seq-in) seq-out
        (contains? seen-values current) seq-out
        :else (recur (conj seen-values current) (rest seq-in) (conj seq-out current))))))
