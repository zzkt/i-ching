;;; i-ching.el --- The Book of Changes -*- coding: utf-8; lexical-binding: t -*-

;; Copyright 2001, 2020 FoAM
;;
;; Author: nik gaffney <nik@fo.am>
;; Created: 2001-01-24
;; Version: 0.2
;; Package-Requires: ((emacs "25.1") (request "0.3"))
;; Keywords: games, divination, stochastism, cleromancy, change
;; URL:  https://github.com/zzkt/i-ching

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; The I Ching or Book of Changes can be used as a divination method,
;; pattern generator or fixed point for millennia of commentary & exegesis.
;;
;; This package provides methods for casting and describing hexagrams,
;; querying the oracle, and finding patterns in randomness.
;;
;; The descriptions of hexagrams and their classification have been drawn from
;; public domain sources, tradition and antiquity. Further details of usage
;; along with reading & study material can be found in the README file.


;;; History:
;; - initial text, 西周 (1000–750 BC)
;; - [...]
;; - transliteration to emacs on Lunar New Year, Metal Snake
;; - recast during the Month of Hungry Ghosts 2020 CE
;; - Ox/West. Cold Moon. 2020-12-30 CE


;;; Code:
(require 'request)

(defgroup i-ching '()
  "Cast hexagrams and consult the I Ching."
  :prefix "i-ching-"
  :group 'stochastism)

;; configuration and customisation

(defcustom i-ching-divination-method '3-coins
  "The method for producing a hexagram.

   Various methods can be used, each has it's own particulars
       3-coins: simulates 3 coins (default)
       4-coins: (unimplemented)
 yarrow-stalks: simulate bundles of yarrow stalks
         6-bit: use a single random number
   cheezburger: hypothetical method devised to generate hexagrams
                from locat gifs (unimplemented)"
  :type  '(choice
           (symbol :tag "simulate 3 coins method" 3-coins)
           (symbol :tag "simulate yarrow stalks" yarrow-stalks)
           (symbol :tag "use a single random number" 6-bit)
           (symbol :tag "simulate 4 coins (unimplemented)" 4-coins)))

(defcustom  i-ching-randomness-source 'pseudo
  "The method of drawing upon the universe's unpredictability.

       quantum: use Genuine Quantum Randomness from ANU
        random: use bespoke random numbers from random.org
        pseudo: use locally sourced (pseudo)randomness (default/fallback method)"
  :type  '(choice
           (symbol :tag "Use locally sourced (pseudo)randomness" pseudo)
           (symbol :tag "Use bespoke random numbers from random.org" random)
           (symbol :tag "Use Genuine Quantum Randomness from ANU" quantum)))

(defcustom i-ching-hexagram-size 18 "Font size of Hexagrams."
  :type 'integer)

(defcustom i-ching-hexagram-font "DejaVu Sans" "Font to use for Hexagrams."
  :type 'string)

(defun i-ching-update-fontsize ()
  "Change the font and size of Hexagrams in Unicode range 4DC0—4DFF."
  (interactive)
  (let ((hexagram (font-spec :family i-ching-hexagram-font :size i-ching-hexagram-size)))
    (set-fontset-font t '(#x4DC0 . #x4DFF) hexagram)))


;; various sequences of hexagrams

(defvar i-ching-sequence-king-wen
  '((1  . "䷀") (2  . "䷁") (3  . "䷂") (4  . "䷃") (5  . "䷄") (6  . "䷅") (7  . "䷆") (8  . "䷇")
    (9  . "䷈") (10 . "䷉") (11 . "䷊") (12 . "䷋") (13 . "䷌") (14 . "䷍") (15 . "䷎") (16 . "䷏")
    (17 . "䷐") (18 . "䷑") (19 . "䷒") (20 . "䷓") (21 . "䷔") (22 . "䷕") (23 . "䷖") (24 . "䷗")
    (25 . "䷘") (26 . "䷙") (27 . "䷚") (28 . "䷛") (29 . "䷜") (30 . "䷝") (31 . "䷞") (32 . "䷟")
    (33 . "䷠") (34 . "䷡") (35 . "䷢") (36 . "䷣") (37 . "䷤") (38 . "䷥") (39 . "䷦") (40 . "䷧")
    (41 . "䷨") (42 . "䷩") (43 . "䷪") (44 . "䷫") (45 . "䷬") (46 . "䷭") (47 . "䷮") (48 . "䷯")
    (49 . "䷰") (50 . "䷱") (51 . "䷲") (52 . "䷳") (53 . "䷴") (54 . "䷵") (55 . "䷶") (56 . "䷷")
    (57 . "䷸") (58 . "䷹") (59 . "䷺") (60 . "䷻") (61 . "䷼") (62 . "䷽") (63 . "䷾") (64 . "䷿"))
  "The Hexagrams ordered by index in the King Wen sequence.")

(defvar i-ching-sequence-leibniz
  '((#b111111 . "䷀") (#b000000 . "䷁") (#b100010 . "䷂") (#b010001 . "䷃")
    (#b111010 . "䷄") (#b010111 . "䷅") (#b010000 . "䷆") (#b000010 . "䷇")
    (#b111011 . "䷈") (#b110111 . "䷉") (#b111000 . "䷊") (#b000111 . "䷋")
    (#b101111 . "䷌") (#b111101 . "䷍") (#b001000 . "䷎") (#b000100 . "䷏")
    (#b100110 . "䷐") (#b011001 . "䷑") (#b110000 . "䷒") (#b000011 . "䷓")
    (#b100101 . "䷔") (#b101001 . "䷕") (#b000001 . "䷖") (#b100000 . "䷗")
    (#b100111 . "䷘") (#b111001 . "䷙") (#b100001 . "䷚") (#b011110 . "䷛")
    (#b010010 . "䷜") (#b101101 . "䷝") (#b001110 . "䷞") (#b011100 . "䷟")
    (#b001111 . "䷠") (#b111100 . "䷡") (#b000101 . "䷢") (#b101000 . "䷣")
    (#b101011 . "䷤") (#b110101 . "䷥") (#b001010 . "䷦") (#b010100 . "䷧")
    (#b110001 . "䷨") (#b100011 . "䷩") (#b111110 . "䷪") (#b011111 . "䷫")
    (#b000110 . "䷬") (#b011000 . "䷭") (#b010110 . "䷮") (#b011010 . "䷯")
    (#b101110 . "䷰") (#b011101 . "䷱") (#b100100 . "䷲") (#b001001 . "䷳")
    (#b001011 . "䷴") (#b110100 . "䷵") (#b101100 . "䷶") (#b001101 . "䷷")
    (#b011011 . "䷸") (#b110110 . "䷹") (#b010011 . "䷺") (#b110010 . "䷻")
    (#b110011 . "䷼") (#b001100 . "䷽") (#b101010 . "䷾") (#b010101 . "䷿"))
  "The King Wen sequence of Hexagrams in binary (Leibniz) representation.")


;;;;;;;;;;;;; ; ; ; ;;;       ;      ;
;;
;;  Hexagram details have been extracted from Wikipedia (CC-BY-SA) and the Unicode Standard.
;;
;;  Several variants on the names and descriptions are included, with "The Judgment"
;;  (or "Decision") and "The Image" for each hexagram based on either the unicode name,
;;  Wilhelm or Legge translation. Most of the descriptions have been updated to reflect
;;  current preferences for gender-neutral and imperative mood (mostly) in line with Pearson.
;;
;;  None of the commentaries, commentaries on commentaries, wings, or notes on the
;;  individual lines are included for the sake of brevity. However it should be obvious
;;  how to cross-reference the commentaries given the hexagram number and translation
;;  (e.g. Legge, Wilhem-Baynes, Huang, Pearson, Blofeld, Liu, Siu, Wing. etc)
;;
;;  There remains room for further automation...
;;
;;;;;;;;;;; ; ;;;

(defcustom i-ching-junzi "you should"
  "The preferred English translation of junzi 君子.
c.f. ‘The Superior man’ (Legge, Wilhem), ‘Noble young one’ (Hatcher), 'the True Self' (Anthony), 'you should' (Pearson), with reference to “Conventions Used by This Translation” (Pearson)."
  :type '(string))

(defvar i-ching-hexagram-summary
  '((1 "䷀"
       "Hexagram 1 is named 乾 (qián), “Force”. Other variations include “the creative”, “strong action”, “the key”, and “initiating”. Its inner (lower) trigram is ☰ (乾 qián) force = (天) heaven, and its outer (upper) trigram is the same."
       "THE CREATIVE works sublime success, Furthering through perseverance."
       "The movement of heaven is full of power. Thus, you should become strong and untiring."
       "HEXAGRAM FOR THE CREATIVE HEAVEN")
    (2 "䷁"
       "Hexagram 2 is named 坤 (kūn), “Field”. Other variations include “the receptive”, “acquiescence”, and “the flow”. Its inner (lower) trigram is ☷ (坤 kūn) field = (地) earth, and its outer (upper) trigram is identical."
       "THE RECEPTIVE brings about sublime success, Furthering through the perseverance of a mare. If you undertake something and try to lead, things go astray; But if you should follow, you will find guidance. It is favorable to find friends in the west and south, To forego friends in the east and north. Quiet perseverance brings good fortune."
       "The earth's condition is receptive devotion. Thus the person who has breadth of character carries the outer world."
       "HEXAGRAM FOR THE RECEPTIVE EARTH")
    (3 "䷂"
       "Hexagram 3 is named 屯 (zhūn), “Sprouting”. Other variations include “difficulty at the beginning”, “gathering support”, and “hoarding”. Its inner (lower) trigram is ☳ (震 zhèn) shake = (雷) thunder, and its outer (upper) trigram is ☵ (坎 kǎn) gorge = (水) water."
       "DIFFICULTY AT THE BEGINNING works supreme success, Furthering through perseverance. Nothing should be undertaken. It furthers you to appoint helpers."
       "Clouds and thunder: The image of DIFFICULTY AT THE BEGINNING. Thus, you should bring order out of confusion."
       "HEXAGRAM FOR DIFFICULTY AT THE BEGINNING")
    (4 "䷃"
       "Hexagram 4 is named 蒙 (méng), “Enveloping”. Other variations include “youthful folly”, “the young shoot”, and “discovering”. Its inner trigram is ☵ (坎 kǎn) gorge = (水) water. Its outer trigram is ☶ (艮 gèn) bound = (山) mountain."
       "YOUTHFUL FOLLY has success. It is not I who seek the young fool; The young fool seeks me. At the first oracle I inform him. If he asks two or three times, it is importunity. If he importunes, I give him no information. Perseverance furthers."
       "A spring wells up at the foot of the mountain: The image of YOUTH. Thus, you should foster your character by thoroughness in all that you do."
       "HEXAGRAM FOR YOUTHFUL FOLLY")
    (5 "䷄"
       "Hexagram 5 is named 需 (xū), “Attending”. Other variations include “waiting”, “moistened”, and “arriving”. Its inner (lower) trigram is ☰ (乾 qián) force = (天) heaven, and its outer (upper) trigram is ☵ (坎 kǎn) gorge = (水) water."
       "WAITING. If you are sincere, you have light and success. Perseverance brings good fortune. It furthers one to cross the great water."
       "Clouds rise up to heaven: The image of WAITING. Thus, you should eats and drink, be joyous and of good cheer."
       "HEXAGRAM FOR WAITING")
    (6 "䷅"
       "Hexagram 6 is named 訟 (sòng), “Arguing”. Other variations include “conflict” and “lawsuit”. Its inner (lower) trigram is ☵ (坎 kǎn) gorge = (水) water, and its outer (upper) trigram is ☰ (乾 qián) force = (天) heaven."
       "CONFLICT. You are sincere and are being obstructed. A cautious halt halfway brings good fortune. Going through to the end brings misfortune. You should seek advice from someone greater. It will be ineffective to attempt major change."
       "Heaven and water go their opposite ways: The image of CONFLICT. Thus in all your transactions you should carefully consider the beginning."
       "HEXAGRAM FOR CONFLICT")
    (7 "䷆"
       "Hexagram 7 is named 師 (shī), “Leading”. Other variations include “the army” and “the troops”. Its inner (lower) trigram is ☵ (坎 kǎn) gorge = (水) water, and its outer (upper) trigram is ☷ (坤 kūn) field = (地) earth."
       "THE ARMY needs perseverance and a strong leader. Good fortune without blame."
       "In the middle of the earth is water: The image of THE ARMY. The Leader nourishes and educates the people, and collects from among them a mighty army."
       "HEXAGRAM FOR THE ARMY")
    (8 "䷇"
       "Hexagram 8 is named 比 (bǐ), “Grouping”. Other variations include “holding together” and “alliance”. Its inner (lower) trigram is ☷ (坤 kūn) field = (地) earth, and its outer (upper) trigram is ☵ (坎 kǎn) gorge = (水) water."
       "HOLDING TOGETHER brings good fortune. Inquire of the oracle once again whether you possess sublimity, constancy, and perseverance; then there is no blame. Those who are uncertain gradually join. Whoever arrive too late meets with misfortune."
       "On the earth is water: The image of HOLDING TOGETHER. Thus the kings of antiquity Bestowed the different states as fiefs and cultivated friendly relations with their feudal lords."
       "HEXAGRAM FOR HOLDING TOGETHER")
    (9 "䷈"
       "Hexagram 9 is named 小畜 (xiǎo chù), “Small Accumulating”. Other variations include “the taming power of the small” and “small harvest”. Its inner (lower) trigram is ☰ (乾 qián) force = (天) heaven, and its outer (upper) trigram is ☴ (巽 xùn) ground = (風) wind."
       "THE TAMING POWER OF THE SMALL Has success. Dense clouds, no rain from our western region."
       "The wind drives across heaven: The image of THE TAMING POWER OF THE SMALL. Thus refine the outward aspect of your nature."
       "HEXAGRAM FOR SMALL TAMING")
    (10 "䷉"
        "Hexagram 10 is named 履 (lǚ), “Treading”. Other variations include “treading (conduct)” and “continuing”. Its inner (lower) trigram is ☱ (兌 duì) open = (澤) swamp, and its outer (upper) trigram is ☰ (乾 qián) force = (天) heaven."
        "TREADING. Treading upon the tail of the tiger. It does not bite. Success."
        "The image of the sky above, and below it the waters of a marsh. Cautious Advance. You should, accordingly, determine what is high and low, and give settlement to the aims of the people."
        "HEXAGRAM FOR TREADING")
    (11 "䷊"
        "Hexagram 11 is named 泰 (tài), “Pervading”. Other variations include “peace” and “greatness”. Its inner (lower) trigram is ☰ (乾 qián) force = (天) heaven, and its outer (upper) trigram is ☷ (坤 kūn) field = (地) earth."
        "PEACE. The small departs, The great approaches. Good fortune. Success."
        "Heaven and earth unite: the image of PEACE. Thus the ruler divides and completes the course of heaven and earth, and so aids the people."
        "HEXAGRAM FOR PEACE")
    (12 "䷋"
        "Hexagram 12 is named 否 (pǐ), “Obstruction”. Other variations include “standstill (stagnation)” and “selfish persons”. Its inner (lower) trigram is ☷ (坤 kūn) field = (地) earth, and its outer (upper) trigram is ☰ (乾 qián) force = (天) heaven."
        "STANDSTILL. Evil people do not further your perseverance. The great departs; the small approaches."
        "Heaven and earth do not unite: The image of STANDSTILL. Thus, you should fall back upon your inner worth to escape the difficulties, withdrawing from evil, and refusing both honor and wealth."
        "HEXAGRAM FOR STANDSTILL")
    (13 "䷌"
        "Hexagram 13 is named 同人 (tóng rén), “Concording People”. Other variations include “fellowship with men” and “gathering men”. Its inner (lower) trigram is ☲ (離 lí) radiance = (火) fire, and its outer (upper) trigram is ☰ (乾 qián) force = (天) heaven."
        "FELLOWSHIP in the open. Success. It will be advantageous to cross the great river. It will be advantageous to maintain firm correctness."
        "Heaven together with fire: The image of FELLOWSHIP. Thus, you should distinguish things according to their kinds and classes, and make distinctions between things."
        "HEXAGRAM FOR FELLOWSHIP")
    (14 "䷍"
        "Hexagram 14 is named 大有 (dà yǒu), “Great Possessing”. Other variations include “possession in great measure” and “the great possession”. Its inner (lower) trigram is ☰ (乾 qián) force = (天) heaven, and its outer (upper) trigram is ☲ (離 lí) radiance = (火) fire."
        "POSSESSION IN GREAT MEASURE. Supreme success."
        "Fire in heaven above: the image of POSSESSION IN GREAT MEASURE. Thus, you should suppress evil and nurture virtue in accordance with the benevolent will of heaven."
        "HEXAGRAM FOR GREAT POSSESSION")
    (15 "䷎"
        "Hexagram 15 is named 謙 (qiān), “Humbling”. Other variations include “modesty”. Its inner (lower) trigram is ☶ (艮 gèn) bound = (山) mountain and its outer (upper) trigram is ☷ (坤 kūn) field = (地) earth."
        "MODESTY creates success. You should follow things through."
        "Within the earth, a mountain: The image of MODESTY. Thus, you should reduce that which is too much, and augment that which is too little. Weigh things and make them equal."
        "HEXAGRAM FOR MODESTY")
    (16 "䷏"
        "Hexagram 16 is named 豫 (yù), “Providing-For”. Other variations include “enthusiasm” and “excess”. Its inner (lower) trigram is ☷ (坤 kūn) field = (地) earth, and its outer (upper) trigram is ☳ (震 zhèn) shake = (雷) thunder."
        "ENTHUSIASM. It furthers one to install helpers and to set armies marching."
        "Thunder comes resounding out of the earth: The image of ENTHUSIASM. Thus the ancient kings made music in order to honour virtue, and offered it with splendour to the Supreme Deity, inviting their ancestors to be present."
        "HEXAGRAM FOR ENTHUSIASM")
    (17 "䷐"
        "Hexagram 17 is named 隨 (suí), “Following”. Its inner (lower) trigram is ☳ (震 zhèn) shake = (雷) thunder, and its outer (upper) trigram is ☱ (兌 duì) open = (澤) swamp."
        "FOLLOWING has supreme success. Perseverance furthers. No blame."
        "Thunder in the middle of the lake: The image of FOLLOWING. Thus, you should go indoors at nightfall to rest."
        "HEXAGRAM FOR FOLLOWING")
    (18 "䷑"
        "Hexagram 18 is named 蠱 (gǔ), “Repair”. Other variations include “work on what has been spoiled (decay)”, Correcting, misdeeds, decaying and “branch”.[1] Its inner (lower) trigram is ☴ (巽 xùn) ground = (風) wind, and its outer (upper) trigram is ☶ (艮 gèn) bound = (山) mountain. Gu is the name of a venom-based poison traditionally used in Chinese witchcraft."
        "Successful progress is indicated for those who properly repair what has been spoiled. It is advantageous to cross the great river. You should consider carefully the events three days before the turning point and the tasks remaining for three days afterwards."
        "The wind blows low on the mountain: The image of DECAY. Thus, you should encourage people and strengthen their spirit."
        "HEXAGRAM FOR WORK ON THE DECAYED")
    (19 "䷒"
        "Hexagram 19 is named 臨 (lín), “Nearing”. Other variations include “approach” and “the forest”. Its inner (lower) trigram is ☱ (兌 duì) open = (澤) swamp, and its outer (upper) trigram is ☷ (坤 kūn) field = (地) earth."
        "APPROACH has supreme success. Perseverance. In the Eighth month, there will be misfortune."
        "The earth above the lake: The image of APPROACH. Thus become inexhaustible in your instruction and unflagging in your nourishing support of people."
        "HEXAGRAM FOR APPROACH")
    (20 "䷓"
        "Hexagram 20 is named 觀 (guān), “Viewing”. Other variations include “contemplation (view)” and “looking up”. Its inner (lower) trigram is ☷ (坤 kūn) field = (地) earth, and its outer (upper) trigram is ☴ (巽 xùn) ground = (風) wind."
        "CONTEMPLATION. The ablution has been made, but not yet the offering. Full of trust they look up to him."
        "The wind blows over the earth: The image of CONTEMPLATION. Thus the kings of old visited the regions of the world, contemplated the people, and gave them instruction."
        "HEXAGRAM FOR CONTEMPLATION")
    (21 "䷔"
        "Hexagram 21 is named 噬嗑 (shì kè), “Gnawing Bite”. Other variations include “biting through” and “biting and chewing”. Its inner (lower) trigram is ☳ (震 zhèn) shake = (雷) thunder, and its outer trigram is ☲ (離 lí) radiance = (火) fire."
        "BITING THROUGH has success. It is favorable to let justice be administered."
        "Thunder and lighting: The image of BITING THROUGH. Thus the kings of former times made firm the laws through clearly defined penalties."
        "HEXAGRAM FOR BITING THROUGH")
    (22 "䷕"
        "Hexagram 22 is named 賁 (bì), “Adorning”. Other variations include “grace” and “luxuriance”. Its inner (lower) trigram is ☲ (離 lí) radiance = (火) fire, and its outer (upper) trigram is ☶ (艮 gèn) bound = (山) mountain. [3]"
        "GRACE has success. In small matters It is favorable to undertake something."
        "Fire at the foot of the mountain: The image of GRACE. Thus, you should proceed with clearing up current affairs. But do not decide controversial issues in this way."
        "HEXAGRAM FOR GRACE")
    (23 "䷖"
        "Hexagram 23 is named 剝 (bō), “Stripping”. Other variations include “splitting apart” and “flaying”. Its inner trigram is ☷ (坤 kūn) field = (地) earth, and its outer trigram is ☶ (艮 gèn) bound = (山) mountain."
        "SPLITTING APART. IT does not further one to go anywhere."
        "The mountain rests on the earth: The image of SPLITTING APART. Thus those above can ensure their position only by giving generously to those below."
        "HEXAGRAM FOR SPLITTING APART")
    (24 "䷗"
        "Hexagram 24 is named 復 (fù), “Returning”. Other variations include “return (the turning point)”. Its inner trigram is ☳ (震 zhèn) shake = (雷) thunder, and its outer trigram is ☷ (坤 kūn) field = (地) earth."
        "RETURN. Success. Going out and coming in without error. Friends come without blame. To and fro goes the way. On the seventh day comes return. It furthers one to have somewhere to go."
        "Thunder within the earth: The image of THE TURNING POINT. Thus the kings of antiquity closed the passes at the time of solstice. Merchants and strangers did not go about, and the ruler did not travel through the provinces."
        "HEXAGRAM FOR RETURN")
    (25 "䷘"
        "Hexagram 25 is named 無妄 (wú wàng), “Without Embroiling”. Other variations include “innocence (the unexpected)” and “pestilence”. Its inner trigram is ☳ (震 zhèn) shake = (雷) thunder, and its outer trigram is ☰ (乾 qián) force = (天) heaven."
        "INNOCENCE. Supreme success. Perseverance furthers. If someone is not as he should be, He has misfortune, And it does not further him To undertake anything."
        "Under heaven thunder rolls: All things attain the natural state of innocence. Thus the kings of old, rich in virtue and in harmony with the time, fostered and nourished all beings."
        "HEXAGRAM FOR INNOCENCE")
    (26 "䷙"
        "Hexagram 26 is named 大畜 (dà chù), “Great Accumulating”. Other variations include “the taming power of the great”, “great storage”, and “potential energy”. Its inner trigram is ☰ (乾 qián) force = (天) heaven, and its outer trigram is ☶ (艮 gèn) bound = (山) mountain."
        "THE TAMING POWER OF THE GREAT. Perseverance furthers. Not eating at home brings good fortune. It furthers one to cross the great water."
        "Heaven within the mountain: The image of THE TAMING POWER OF THE GREAT. Thus, you should acquaint yourself with the sayings of antiquity and many deeds of the past in order to strengthen your character."
        "HEXAGRAM FOR GREAT TAMING")
    (27 "䷚"
        "Hexagram 27 is named 頤 (yí), “Swallowing”. Other variations include “the corners of the mouth (providing nourishment)”, “jaws” and “comfort/security”. Its inner trigram is ☳ (震 zhèn) shake = (雷) thunder, and its outer trigram is ☶ (艮 gèn) bound = (山) mountain."
        "THE CORNERS OF THE MOUTH. Perseverance brings good fortune. Pay heed to the providing of nourishment And to what a man seeks To fill his own mouth with."
        "At the foot of the mountain, thunder: The image of PROVIDING NOURISHMENT. Thus, you should be careful of your words and temperate in eating and drinking."
        "HEXAGRAM FOR MOUTH CORNERS")
    (28 "䷛"
        "Hexagram 28 is named 大過 (dà guò), “Great Exceeding”. Other variations include “preponderance of the great”, “great surpassing” and “critical mass”. Its inner trigram is ☴ (巽 xùn) ground = (風) wind, and its outer trigram is ☱ (兌 duì) open = (澤) swamp."
        "PREPONDERANCE OF THE GREAT. The ridgepole sags to the breaking point. It furthers one to have somewhere to go. Success."
        "The lake rises above the trees: The image of PREPONDERANCE OF THE GREAT. When you stand alone, be unconcerned, and if you must renounce the world, be undaunted."
        "HEXAGRAM FOR GREAT PREPONDERANCE")
    (29 "䷜"
        "Hexagram 29 is named 坎 (kǎn), “Gorge”. Other variations include “the abyss” (in the oceanographic sense) and “repeated entrapment”. Its inner trigram is ☵ (坎 kǎn) gorge = (水) water, and its outer trigram is identical."
        "THE ABYSMAL repeated. If you are sincere, you have success in your heart, And whatever you do succeeds."
        "Water flows on uninterruptedly and reaches its goal: The image of THE ABYSMAL (WATER). Walk in lasting virtue and carry on the business of teaching."
        "HEXAGRAM FOR THE ABYSMAL WATER")
    (30 "䷝"
        "Hexagram 30 is named 離 (lí), “Radiance”. Other variations include “the clinging, fire” and “the net”. Its inner trigram is ☲ (離 lí) radiance = (火) fire, and its outer trigram is identical. The origin of the character has its roots in symbols of long-tailed birds such as the peacock or the legendary phoenix."
        "THE CLINGING. Perseverance furthers. It brings success. Care of the cow brings good fortune."
        "That which is bright rises twice: The image of FIRE. Thus, by perpetuating this brightness, you should illuminate the world."
        "HEXAGRAM FOR THE CLINGING FIRE")
    (31 "䷞"
        "Hexagram 31 is named 咸 (xián), “Initiative (Influence)”. Other variations include “influence (wooing)” and “feelings”. Its inner trigram is ☶ (艮 gèn) bound = (山) mountain, and its outer trigram is ☱ (兌 duì) open = (澤) swamp."
        "Upon fulfillment of the conditions implied in INITIATIVE, there will be free course and success. Advantage depends upon firm correctness, as in marrying a young lady. Good fortune."
        "A lake on the mountain: The image of INFLUENCE. You should encourage people to approach you by your readiness to receive them."
        "HEXAGRAM FOR INFLUENCE")
    (32 "䷟"
        "Hexagram 32 is named 恆 (héng), “Persevering”. Other variations include “duration” and “constancy”. Its inner trigram is ☴ (巽 xùn) ground = (風) wind, and its outer trigram is ☳ (震 zhèn) shake = (雷) thunder."
        "DURATION. Success. No blame. Progress without error through firm correctness. Movement in any direction is advantageous."
        "Thunder and wind: the image of DURATION. So stand firm and do not change direction."
        "HEXAGRAM FOR DURATION")
    (33 "䷠"
        "Hexagram 33 is named 遯 (dùn), “Retiring”. Other variations include “retreat”, “yielding”, Withdrawal, Retiring, Wielding, Strategic Withdrawal, Inaccessibility, Disassociation from Inferior Forces. Its inner trigram is ☶ (艮 gèn) bound = (山) mountain, and its outer trigram is ☰ (乾 qián) force = (天) heaven."
        "RETREAT. Success. Advantage comes from firm correctness and attention to details."
        "Mountain under heaven: the image of RETREAT. Thus, you should keep people at a distance by dignified bearing rather than hostility."
        "HEXAGRAM FOR RETREAT")
    (34 "䷡"
        "Hexagram 34 is named 大壯 (dà zhuàng), “Great Invigorating”. Other variations include “the power of the great”, great maturity. Its inner trigram is ☰ (乾 qián) force = (天) heaven, and its outer trigram is ☳ (震 zhèn) shake = (雷) thunder."
        "GREAT POWER necessitates firm correctness."
        "Thunder in heaven above: The image of THE POWER OF THE GREAT. Do not take any step that is not in accordance with propriety."
        "HEXAGRAM FOR GREAT POWER")
    (35 "䷢"
        "Hexagram 35 is named 晉 (jìn), “Prospering”. Other variations include “progress” and “aquas”. Its inner trigram is ☷ (坤 kūn) field = (地) earth, and its outer trigram is ☲ (離 lí) radiance = (火) fire."
        "PROGRESS. The powerful prince is honored with horses in large numbers. In a single day he is granted audience three times."
        "The sun rises over the earth: The image of PROGRESS. Thus, you should increase the brightness of your bright virtue."
        "HEXAGRAM FOR PROGRESS")
    (36 "䷣"
        "Hexagram 36 is named 明夷 (míng yí), “Darkening of the Light”. Other variations include “brilliance injured” and “intelligence hidden”. Its inner trigram is ☲ (離 lí) radiance = (火) fire, and its outer trigram is ☷ (坤 kūn) field = (地) earth."
        "DARKENING OF THE LIGHT. Be aware of the difficulty of your position."
        "The light has sunk into the earth: The image of DARKENING OF THE LIGHT. Thus, veil your light yet still shine."
        "HEXAGRAM FOR DARKENING OF THE LIGHT")
    (37 "䷤"
        "Hexagram 37 is named 家人 (jiā rén), “Dwelling People”. Other variations include “the family (the clan)”, “family members”, Family Life, Clan, Home, Linkage, The Psyche. Its inner trigram is ☲ (離 lí) radiance = (火) fire, and its outer trigram is ☴ (巽 xùn) ground = (風) wind."
        "THE FAMILY. Perseverance."
        "Wind comes forth from fire: The image of THE FAMILY. Thus, you should speak the truth and be consistent in your behaviour."
        "HEXAGRAM FOR THE FAMILY")
    (38 "䷥"
        "Hexagram 38 is named 睽 (kuí), “Mutual Alienation”. Other variations include “opposition”, Polarising, The Symbol of Strangeness and Disunion, The Estranged, Opposites, Polarizing, Alienation, Distant From, Perversion, Disharmony, Separated, Contradiction, Estrangement, Incongruity . Its inner trigram is ☱ (兌 duì) open = (澤) swamp, and its outer trigram is ☲ (離 lí) radiance = (火) fire."
        "OPPOSITION. In small matters, good fortune."
        "Above, fire; below. The lake, the image of OPPOSITION. Accept the diversities which make up the whole."
        "HEXAGRAM FOR OPPOSITION")
    (39 "䷦"
        "Hexagram 39 is named 蹇 (jiǎn), “Limping”. Other variations include “obstruction” and “afoot”. Its inner trigram is ☶ (艮 gèn) bound = (山) mountain, and its outer trigram is ☵ (坎 kǎn) gorge = (水) water."
        "During an Impasse advantage is found in the southwest, disadvantage in the northeast. See the great man. Firm correctness brings good fortune."
        "Water on the mountain: The image of OBSTRUCTION. Thus turn your attention to yourself and mould your character."
        "HEXAGRAM FOR OBSTRUCTION")
    (40 "䷧"
        "Hexagram 40 is named 解 (xiè), “Liberation”. Other variations include Deliverance, The Symbol of Loosening, Release, Eliminating Obstacles, Taking-apart, Untangled, Solution, Dissolution, Relief, Unloose, Release of Tension. Its inner trigram is ☵ (坎 kǎn) gorge = (水) water, and its outer trigram is ☳ (震 zhèn) shake = (雷) thunder."
        "Liberation finds advantage in the southwest. When the operation is completed, a return to stability brings good fortune. If operations are incomplete, it is best to finish them quickly."
        "Thunder and rain set in: The image of DELIVERANCE. In accordance with this, forgive errors and deal gently with misdeeds."
        "HEXAGRAM FOR DELIVERANCE")
    (41 "䷨"
        "Hexagram 41 is named 損 (sǔn), “Diminishing”. Other variations include “decrease”. Its inner trigram is ☱ (兌 duì) open = (澤) swamp, and its outer trigram is ☶ (艮 gèn) bound = (山) mountain."
        "DECREASE sincerely maintained rectitude brings great success. Action is appropriate if one's sacrifice is sincere. How is this to be carried out?"
        "At the foot of the mountain, the lake: The image of DECREASE. You should restrains your wrath and repress your desires."
        "HEXAGRAM FOR DECREASE")
    (42 "䷩"
        "Hexagram 42 is named 益 (yì), “Increase”. Other variations include The Symbol of Addition, Gain, Augmenting, Help from Above, Benefit, Advantage, Profit, Expansion. Its inner trigram is ☳ (震 zhèn) shake = (雷) thunder, and its outer trigram is ☴ (巽 xùn) ground = (風) wind."
        "INCREASE. There is advantage in every movement which shall be undertaken, it will even be advantageous to cross the great river."
        "Wind and thunder: the image of INCREASE. Thus, if you perceive good move toward it and when you perceive your own faults, eliminate them."
        "HEXAGRAM FOR INCREASE")
    (43 "䷪"
        "Hexagram 43 is named 夬 (guài), “Displacement”. Other variations include “resoluteness”, “parting”, and “break-through”. Its inner trigram is ☰ (乾 qián) force = (天) heaven, and its outer trigram is ☱ (兌 duì) open = (澤) swamp."
        "BREAKTHROUGH. Recognizing the risks involved in criminal prosecution, justice demands a resolute proof of the culprit's guilt in courts. Inform the city that armed force is not necessary. In this way progress is assured."
        "The lake has risen up to heaven: The image of BREAKTHROUGH. In accordance with this, you should not hoard your wealth, but shares it with others"
        "HEXAGRAM FOR BREAKTHROUGH")
    (44 "䷫"
        "Hexagram 44 is named 姤 (gòu), “Temptation”. Other variations include Coming to Meet, The Symbol of Meeting, Contact, Encountering, Coupling, Adultery. Its inner trigram is ☴ (巽 xùn) ground = (風) wind, and its outer trigram is ☰ (乾 qián) force = (天) heaven."
        "COMING TO MEET a woman who is bold and strong. It will not be good to marry her."
        "Under heaven, wind: The image of Temptation. The sovereign delivers his charges, and promulgates his announcements throughout the four quarters of the kingdom."
        "HEXAGRAM FOR COMING TO MEET")
    (45 "䷬"
        "Hexagram 45 is named 萃 (cuì), “Clustering”. Other variations include “Gathering Together (Contraction)” and “finished”. Its inner trigram is ☷ (坤 kūn) field = (地) earth, and its outer trigram is ☱ (兌 duì) open = (澤) swamp."
        "GATHERING TOGETHER. Success. For successful progress maintain firm correctness. A large sacrifice brings good fortune, proceed toward your destination."
        "Over the earth, the lake: The image of GATHERING TOGETHER. Thus, you should gather your weapons in readiness for the unexpected."
        "HEXAGRAM FOR GATHERING TOGETHER")
    (46 "䷭"
        "Hexagram 46 is named 升 (shēng), “Pushing Upward”. Other variations include Rising and Advancing, Ascending, Ascension, Rising, Promotion, Advancement, Sprouting from the Earth, Organic Growth. Its inner trigram is ☴ (巽 xùn) ground = (風) wind, and its outer trigram is ☷ (坤 kūn) field = (地) earth."
        "PUSHING UPWARD means successful progress. Have no anxiety about meeting with important people. An advance to the south is fortunate."
        "Within the earth, wood grows: The image of PUSHING UPWARD. You should accumulate small things until they become significant."
        "HEXAGRAM FOR PUSHING UPWARD")
    (47 "䷮"
        "Hexagram 47 is named 困 (kùn), “Confining”. Other variations include “oppression (exhaustion)” and “entangled”. Its inner trigram is ☵ (坎 kǎn) gorge = (水) water, and its outer trigram is ☱ (兌 duì) open = (澤) swamp."
        "OPPRESSION. Successful progress is still possible. The perseverance of the truly great brings good fortune without error; but if you rely on words, no one will believe them."
        "There is not water in the lake: The image of EXHAUSTION. Be prepared to sacrifice your life to attain your purpose."
        "HEXAGRAM FOR OPPRESSION")
    (48 "䷯"
        "Hexagram 48 is named 井 (jǐng), “The Well”. Other variations include Welling, Potentialities Fulfilled, The Source, The Deep Psyche. Its inner trigram is ☴ (巽 xùn) ground = (風) wind, and its outer trigram is ☵ (坎 kǎn) gorge = (水) water."
        "THE WELL. The town may be changed but The Well remains the same. Its water level neither disappears nor receives any great increase, and the people can draw from it freely. Misfortune ensues if the rope breaks or the bucket is broken before it reaches the water."
        "Water over wood: the image of THE WELL. Thus, you should comfort people and stimulate their mutual cooperation."
        "HEXAGRAM FOR THE WELL")
    (49 "䷰"
        "Hexagram 49 is named 革 (gé), “Metamorphosis”. Other variations include “Revolution (molting)” Transformation, Skinning, The Bridle, The Symbol of Change, Molting, Leather, Skin, Molt, Cut Off, Changing, Radical Change, Overthrowing. Its inner trigram is ☲ (離 lí) radiance = (火) fire, and its outer trigram is ☱ (兌 duì) open = (澤) swamp."
        "Metamorphosis is believed in only after it has been accomplished. Firm correctness abolishes regret and brings successful progress."
        "Fire in the lake: the image of REVOLUTION. You should synchronise your astronomical calculations to clarify the times and seasons."
        "HEXAGRAM FOR REVOLUTION")
    (50 "䷱"
        "Hexagram 50 is named 鼎 (dǐng), “The Cauldron”. Other variations include The Sacrificial Vessel, Rejuvenation, Cosmic Order, The Alchemical Vessel. Its inner trigram is ☴ (巽 xùn) ground = (風) wind, and its outer trigram is ☲ (離 lí) radiance = (火) fire."
        "THE CAULDRON. Great progress and success."
        "Fire over wood: The image of THE CAULDRON. Thus, you should maintain correctness in every situation."
        "HEXAGRAM FOR THE CAULDRON")
    (51 "䷲"
        "Hexagram 51 is named 震 (zhèn), “Shock”. Other variations include “the arousing (shock, thunder)”, Thunder, The Symbol of Startling Movement, Shake, The Beginning of Movement, Shocking, The Thunderclap, Action, Motion, Sudden Change, Surprise! . Both its inner and outer trigrams are ☳ (震 zhèn) shake = (雷) thunder."
        "SHOCK intimates ease and development. When the time of movement which it indicates comes, the subject of the hexagram will be found looking out with apprehension, and yet smiling and talking cheerfully. When the movement like a crash of thunder terrifies all within a hundred miles, he will be like the sincere worshipper who is not startled into dropping his ladle and cup of sacrificial spirits."
        "Thunder repeated: the image of SHOCK. You should remain fearful and apprehensive, cultivate virtue, and examine your faults."
        "HEXAGRAM FOR THE AROUSING THUNDER")
    (52 "䷳"
        "Hexagram 52 is named 艮 (gèn), “Keeping Still”. Other variations include Mountain, The Symbol of Checking and Stopping, Desisting, Stilling, Stillness, Stoppage, Bound, Reposing, Resting, Meditation, Non-action, Stopping, Arresting Movement. Both its inner and outer trigrams are ☶ (艮 gèn) bound = (山) mountain."
        "KEEPING STILL and losing all consciousness of self. When you walk in the courtyard and do not see people, there will be no error."
        "Mountains standing close together: The image of KEEPING STILL. Do not allow your thoughts to go beyond the duties of your immediate circumstances."
        "HEXAGRAM FOR THE KEEPING STILL MOUNTAIN")
    (53 "䷴"
        "Hexagram 53 is named 漸 (jiàn), “Infiltrating”. Other variations include “Development (gradual progress)”, Advancing, Growth, Developing, Gradualness, Dialectical Progression. Its inner trigram is ☶ (艮 gèn) bound = (山) mountain, and its outer trigram is ☴ (巽 xùn) ground = (風) wind."
        "DEVELOPMENT shows the good fortune in attending the marriage of a young lady. Firm correctness brings advantage."
        "On the mountain, a tree: The image of DEVELOPMENT. Thus, you should attain and nourish extraordinary virtue to improve the manners of the people."
        "HEXAGRAM FOR DEVELOPMENT")
    (54 "䷵"
        "Hexagram 54 is named 歸妹 (guī mèi), “Propriety”. Other variations include “the marrying maiden” and “returning maiden”. Its inner trigram is ☱ (兌 duì) open = (澤) swamp, and its outer trigram is ☳ (震 zhèn) shake = (雷) thunder."
        "Propriety indicates that action will be evil, and in no way advantageous."
        "Thunder over the lake: The image of PROPRIETY. Thus, you should have regard for the far-distant end, and know the mischief that may be done at the beginning."
        "HEXAGRAM FOR THE MARRYING MAIDEN")
    (55 "䷶"
        "Hexagram 55 is named 豐 (fēng), “Abundance”. Other variations include Fullness, The Symbol of Prosperity, Greatness, Abounding, Richness, Prolific, Fruitful, Luxuriant, Zenith, Affluence, Correct Action, Lucid Behavior. Its inner trigram is ☲ (離 lí) radiance = (火) fire, and its outer trigram is ☳ (震 zhèn) shake = (雷) thunder."
        "ABUNDANCE has success, progress and development. When the king is enlightened there is no need to fear change. Let him be as the sun at noon."
        "Both thunder and lightning come: The image of ABUNDANCE. Thus, you should decide carefully on judgements and apportion punishment with exactness."
        "HEXAGRAM FOR ABUNDANCE")
    (56 "䷷"
        "Hexagram 56 is named 旅 (lǚ), “Transition”. Other variations include The Wanderer, The Symbol of the Traveler, The Exile, Sojourning, The Newcomer, To Lodge, Travelling, The Stranger, The Traveling Stranger, The Outsider, Wandering, Uncommitted. Its inner trigram is ☶ (艮 gèn) bound = (山) mountain, and its outer trigram is ☲ (離 lí) radiance = (火) fire."
        "TRANSITION means that small attainments are possible. If the traveling stranger is firm and correct, there will be good fortune."
        "Fire on the mountain: The image of THE WANDERER. Thus, you should exert cautious wisdom in punishments, and do not permit prolonged litigation."
        "HEXAGRAM FOR THE WANDERER")
    (57 "䷸"
        "Hexagram 57 is named 巽 (xùn), “Ground”. Other variations include “the gentle (the penetrating, wind)” and “calculations”. Both its inner and outer trigrams are ☴ (巽 xùn) ground = (風) wind."
        "THE GENTLE. Modest success. Look for important people and move in the direction that implies."
        "Wind following wind upon the other: You should articulate your directions and undertakes your work."
        "HEXAGRAM FOR THE GENTLE WIND")
    (58 "䷹"
        "Hexagram 58 is named 兌 (duì), “Joy”. Other variations include The Joyous, Joyousness, Pleased Satisfaction, Encouraging, Delight, Open, Usurpation, Self-indulgence, Pleasure, Cheerfulness, Frivolity, Callow Optimism. Both its inner and outer trigrams are ☱ (兌 duì) open = (澤) swamp."
        "JOY intimates that under its conditions there will be progress and attainment, but it will be advantageous to be firm and correct."
        "Lakes resting one on the other: The image of JOY. Thus, you should encourage the conversation of friends and the stimulus of their common practice."
        "HEXAGRAM FOR THE JOYOUS LAKE")
    (59 "䷺"
        "Hexagram 59 is named 渙 (huàn), “Expansion (Dispersion)”. Other variations include Dissolution, Disintegration, Dispersal, Overcoming Dissension, Scattering, Dispersing, Unintegrated, Reuniting, Evaporation, Reorganization. Its inner trigram is ☵ (坎 kǎn) gorge = (水) water, and its outer trigram is ☴ (巽 xùn) ground = (風) wind."
        "DISPERSION intimates that there will be progress and success. The king goes to his ancestral temple. It will be advantageous to cross the great river. It will be advantageous to be firm and correct."
        "The wind drives over the water: The image of EXPANSION. The ancient kings, in accordance with this, presented offerings to God and established the ancestral temple."
        "HEXAGRAM FOR DISPERSION")
    (60 "䷻"
        "Hexagram 60 is named 節 (jié), “Limitation”. Other variations include Restraint, Regulations, Articulating, Receipt, Restraining, Containment. Its inner trigram is ☱ (兌 duì) open = (澤) swamp, and its outer trigram is ☵ (坎 kǎn) gorge = (水) water."
        "LIMITATION bring progress and success, but if they are severe and difficult they cannot be permanent."
        "Water over lake: the image of LIMITATION. You should construct methods of numbering and measurement, and examine the nature of virtuous conduct."
        "HEXAGRAM FOR LIMITATION")
    (61 "䷼"
        "Hexagram 61 is named 中孚 (zhōng fú), “Inner Truth”. Other variations include Inward Confidence, Inner Truthfulness, Sincerity, Centering- Conforming, Central Return, Faithfulness in the Center, Sincerity in the Center, Insight, Understanding. Its inner trigram is ☱ (兌 duì) open = (澤) swamp, and its outer trigram is ☴ (巽 xùn) ground = (風) wind."
        "INNER TRUTH moves even pigs and fish, and leads to good fortune. There will be advantage in crossing the great river. There will be advantage in being firm and correct."
        "Wind over lake: the image of INNER TRUTH. Thus, you should deliberate about judgement and delay the infliction of death."
        "HEXAGRAM FOR INNER TRUTH")
    (62 "䷽"
        "Hexagram 62 is named 小過 (xiǎo guò), “Small Powers”. Other variations include Preponderance of the Small, The Symbol of Excess in Small Things, The Small get by, Slight Excess, Small Exceeding, Small Surpassing, Excess of the Small, Small gains, Conscientiousness, Smallness in Excess, Exceeding the Mean, Proliferation of Details. Its inner trigram is ☶ (艮 gèn) bound = (山) mountain, and its outer trigram is ☳ (震 zhèn) shake = (雷) thunder."
        "PREPONDERANCE OF THE SMALL. Success. There will be progress and attainment in small affairs, but not in great affairs. It will be advantageous to be firm and correct. It is like the song of a flying bird: It is better to descend than to ascend. In this way there will be good fortune."
        "Thunder on the mountain: The image of PREPONDERANCE OF THE SMALL. Thus, in your conduct exceed in humility, in mourning exceed in sorrow, and in expenditure exceed in economy."
        "HEXAGRAM FOR SMALL PREPONDERANCE")
    (63 "䷾"
        "Hexagram 63 is named 既濟 (jì jì), “Completion”. Other variations include After Completion, The Symbol of What is Already Past, Already Fording, Already Completed, Settled, Mission Accomplished, Tasks Completed. Its inner trigram is ☲ (離 lí) radiance = (火) fire, and its outer trigram is ☵ (坎 kǎn) gorge = (水) water."
        "COMPLETION intimates progress and success in small matters. There is advantage in firm correctness. There has been good fortune in the beginning; there may be disorder in the end."
        "Water over fire: the image of the condition in COMPLETION. Thus, you should think of the difficulties that may come, and guard against them in advance."
        "HEXAGRAM FOR AFTER COMPLETION")
    (64 "䷿"
        "Hexagram 64 is named 未濟 (wèi jì), “Before Completion”. Other variations include Unfinished Business, Not-yet Fording, Not Yet Completed, Tasks yet to be Completed, Before the End, A State of Transition . Its inner trigram is ☵ (坎 kǎn) gorge = (水) water, and its outer trigram is ☲ (離 lí) radiance = (火) fire."
        "BEFORE COMPLETION. Success. But if the young fox, that has nearly crossed the stream, gets his tail wet there will be no advantage."
        "The image of the condition before transition. Thus, you should carefully discriminate among the qualities of things, so that each can find its place."
        "HEXAGRAM FOR BEFORE COMPLETION"))
  "The Hexagrams. Their name, description, judgment and image. Basis for interpretation.")


;;;;;; ;;;;;   ; ;  ; ; ; ; ;   ;
;;
;;  Hexagram ⟷ Hexagram.
;;          Conversions between representations.
;;
;;;;; ; ; ;


(defun i-ching-number-to-hexagram (number)
  "Convert a NUMBER from the King Wen sequence to a unicode hexagram."
  (let ((hexagrams i-ching-sequence-king-wen))
    (alist-get number hexagrams)))

(defun i-ching-binary-to-hexagram (number)
  "Convert a binary NUMBER to a unicode hexagram (in Lower → Upper line order)."
  (let ((hexagrams i-ching-sequence-leibniz))
    (alist-get number hexagrams)))

(defun i-ching-hexagram-to-number (hexagram)
  "Show the number of HEXAGRAM in the King Wen sequence."
  (let ((hexagrams i-ching-sequence-king-wen))
    (car (rassoc hexagram hexagrams))))

;; textual

(defun i-ching-number-to-description (number)
  "Present an interpretation of Hexagram NUMBER."
  (let ((hexagrams i-ching-hexagram-summary))
    (nth 1 (alist-get number hexagrams))))

(defun i-ching-number-to-judgment (number)
  "Present an interpretation (judgment) of Hexagram NUMBER."
  (let ((hexagrams i-ching-hexagram-summary))
    (nth 2 (alist-get number hexagrams))))

(defun i-ching-number-to-image (number)
  "Present an interpretation (image) of Hexagram NUMBER."
  (let ((hexagrams i-ching-hexagram-summary))
    (nth 3 (alist-get number hexagrams))))

(defun i-ching-number-to-unicode-name (number)
  "The unicode name of Hexagram NUMBER."
  (let ((hexagrams i-ching-hexagram-summary))
    (nth 4 (alist-get number hexagrams))))

(defun i-ching-number-to-name (number)
  "The name of Hexagram NUMBER based on it's unicode name."
  (let* ((hexagrams i-ching-hexagram-summary)
         (unicode-name (nth 4 (alist-get number hexagrams))))
    (when unicode-name
      (capitalize (seq-drop unicode-name 13)))))

;;;###autoload
(defun i-ching-describe-hexagram (point mark)
  "Show the name of a HEXAGRAM (between POINT and MARK) based on it's unicode name."
  (interactive "r")
  (if (use-region-p)
      (let* ((hexagrams i-ching-hexagram-summary)
             (selection (buffer-substring point mark))
             (hexagram
              (nth 4 (alist-get
                      (i-ching-hexagram-to-number selection)
                      hexagrams))))
        (message "%s" (if hexagram
                          (capitalize (seq-drop hexagram 13))
                        "Not a hexagram")))
    (message "Nothing selected")))


;;;;; ;  ; ;       ;
;;
;;  Casting a hexagram
;;
;;; ; ; ;

;;;###autoload
(defun i-ching-cast (&optional method)
  "Cast a Hexagram using a particular METHOD.

There are various methods to divine a specific hexagram.
Depending on the context and circumstance some methods may be more suitable.

see: `i-ching-divination-method' & `i-ching-randomness-source' for details."
  (interactive)
  (let ((method-function
         (pcase method
           ;;  the casting method should return a hexagram (or changing hexagram)
           ('3-coins  #'i-ching--three-coins)
           ('yarrow-stalks #'i-ching--yarrow-stalks)
           ('6-bit #'i-ching--random-number)
           ('4-coins (message "unimplemented"))
           ('bagua (message "unimplemented"))
           ('cheezburger (message "LOL"))
           (_
            #'i-ching--random-number))))
    (when method-function
      (let* ((line-1 (funcall method-function))
             (line-2 (funcall method-function))
             (line-3 (funcall method-function))
             (line-4 (funcall method-function))
             (line-5 (funcall method-function))
             (line-6 (funcall method-function)))
        ;; lines and changing lines (if any)
        (let* ((line-1o (car  line-1))
               (line-1c (cadr line-1))
               (line-2o (car  line-2))
               (line-2c (cadr line-2))
               (line-3o (car  line-3))
               (line-3c (cadr line-3))
               (line-4o (car  line-4))
               (line-4c (cadr line-4))
               (line-5o (car  line-5))
               (line-5c (cadr line-5))
               (line-6o (car  line-6))
               (line-6c (cadr line-6))
               ;; draw the hexagram
               (hexagram (+ line-6o
                            (ash line-5o 1)
                            (ash line-4o 2)
                            (ash line-3o 3)
                            (ash line-2o 4)
                            (ash line-1o 5)))
               ;; draw the changing hexagram
               (hexagram-changing  (+ line-6c
                                      (ash line-5c 1)
                                      (ash line-4c 2)
                                      (ash line-3c 3)
                                      (ash line-2c 4)
                                      (ash line-1c 5))))
          ;; print hexagram
          (message "hexagram: %s%s%s%s%s%s → %s → %s (%s)"
                   line-1o line-2o line-3o line-4o line-5o line-6o
                   hexagram
                   (i-ching-binary-to-hexagram hexagram)
                   (i-ching-binary-to-hexagram hexagram-changing))
          ;; the two hexagrams the same if there are no changing lines
          (if (eql hexagram hexagram-changing)
              (i-ching-binary-to-hexagram hexagram)
            (format "%s→%s"
                    (i-ching-binary-to-hexagram hexagram)
                    (i-ching-binary-to-hexagram hexagram-changing))))))))


;;;###autoload
(defun i-ching-insert-hexagram (&optional number)
  "Insert a hexagram either by casting or it's NUMBER in the King Wen sequence."
  (interactive)
  (insert
   (if number
       (i-ching-number-to-hexagram number)
     (i-ching-cast))))

;;;###autoload
(defun i-ching-insert-hexagram-and-name (&optional number)
  "Insert a hexagram either by casting or it's NUMBER in the King Wen sequence."
  (interactive)
    (let ((hexagram (if number
                      number
                    (i-ching-random 64))))
    (insert (i-ching-hexagram-and-name-string hexagram))))


;;;###autoload
(defun i-ching-hexagram-and-name-string (&optional number)
  "Return a hexagram either by casting or it's NUMBER in the King Wen sequence."
  (interactive)
  (let ((hexagram (if number
                      number
                    (i-ching-random 64))))
    (format "%s %s"
            (i-ching-number-to-hexagram hexagram)
            (i-ching-number-to-name hexagram))))


;;;; ;    ; ;;;;; ;  ;     ;
;;
;;  Sources and methods of randomness
;;
;;; ; ; ;;

(defun i-ching-random (n &optional source)
  "Return a random integer from 1 to N inclusive (possibly with a specific SOURCE of randomness)."
  (when (not source) (setq source i-ching-randomness-source))
  (message "using: %s" source)
  (pcase source
    ('quantum (pcase n
                (64 (i-ching-q64))
                (_  (/ (i-ching-q64) (/ 64 n)))))
    ('atmospheric (pcase n
                    (64 (i-ching-r64))
                    (_  (/ (i-ching-r64) (/ 64 n)))))
    ('pseudo (+ 1 (random n)))
    (_
     (+ 1 (random n)))))


(defun i-ching-q64 ()
  "Genuine Quantum Randomness™️ from quantum fluctuations of the vacuum [1..64].
Provided by ANU QRNG via https://qrng.anu.edu.au/"
  (let ((numeric 0))
    (request
      "https://qrng.anu.edu.au/API/jsonI.php?length=1&type=hex16&size=1"
      :sync t
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setq
                   numeric
                   (/ (string-to-number
                       (elt (alist-get 'data data) 0) 16)
                      4)))))
    numeric))


(defun i-ching-r64 ()
  "True random numbers from atmospheric noise [1..64].
Provided by Randomness and Integrity Services Ltd. via https://www.random.org/"
  (let ((numeric 0))
    (request
      "https://www.random.org/integers/?num=1&min=1&max=64&col=1&base=10&format=plain&rnd=new"
      :sync t
      ;; :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (setf numeric (string-to-number data)))))
    numeric))


;;;;;;;; ; ; ;; ;   ;;   ; ; ;         ;       ;
;;
;;  Methods of divination
;;
;;  Each function should produce a binary pair corresponding to a line of a hexagram
;;  as follows...
;;
;;   (0 1) is yin changing to yang
;;   (1 1) is yang
;;   (0 0) is yin
;;   (1 0) is yang changing to yin
;;
;;;;;;; ;    ; ;

(defun i-ching--random-number ()
  "Random line selection (unchanging)..."
  (pcase (i-ching-random 2)
    (1 '(1 1)) ;; yang
    (2 '(0 0)) ;; yin
    ))

(defun i-ching--coin-toss ()
  "Simulate tossing a  coin where H=2 and T=3."
  (let ((result (+ 1 (i-ching-random 2))))
    (message "coin: %s" (if (= 2 result) "H" "T"))
    result))

(defun i-ching--three-coins ()
  "Simulate 3 coins. The retrn value is a pair of lines."
  (let ((result (+ (i-ching--coin-toss)
                   (i-ching--coin-toss)
                   (i-ching--coin-toss))))
    ;; single hexagram. no changing lines
    (pcase result
      (6 '(0 1)) ;; yin changing
      (7 '(1 1)) ;; yang
      (8 '(0 0)) ;; yin
      (9 '(1 0)) ;; yang changing
      )))

(defun i-ching--yarrow-stalks ()
  "Cast simulated yarrow stalks, return a value for a line, or changing line..."
  (let ((yarrow-stalks 50)
        (west 0)
        (east 0)
        (left 0)
        (group 0)
        (result 0))
    ;; One stalk is set aside and not used again
    (setq yarrow-stalks (1- yarrow-stalks))
    ;; The bundle of the remaining stalks is divided into two bundles
    (dotimes (_ yarrow-stalks)
      (if (= 1 (i-ching-random 2))
          (setq east (1+ east))
        (setq west (1+ west))))
    ;; One stalk is taken from the east bundle and placed between the fingers of the left hand
    (setq east (1- east))
    (setq left (1+ left))
    ;; stalks are removed from the west set aside in groups of 4, until 4 or fewer remain
    (if (= 0 (% west 4)) (setq group 4) (setq group (% west 4)))
    ;; The remaining stalks are placed between the fingers of left hand.
    (setq left (+ left group))
    ;; stalks are removed from the east set aside in groups of 4, until 4 or fewer remain
    (if (= 0 (% east 4)) (setq group 4) (setq group (% east 4)))
    ;; The remaining stalks are placed between the fingers of left hand.
    (setq left (+ left group))
    ;; The number of stalks between the fingers of the left hand are then counted and noted.
    ;; Nine stalks results in the value of 2, five stalks results in the value of 3
    (if (= 9 left) (setq result 2) (setq result 3))

    ;; These stalks are set aside and the remaining bundle of 40 or 44 stalks
    ;; ... is divided and counted out in the same manner.
    (setq yarrow-stalks (- yarrow-stalks left))
    (setq east 0 west 0 left 0 group 0)
    ;; The bundle of the remaining stalks is divided into two bundles
    (dotimes (_ yarrow-stalks)
      (if (= 1 (i-ching-random 2))
          (setq east (1+ east))
        (setq west (1+ west))))
    ;; One stalk is taken from the east bundle and placed between the fingers of the left hand
    (setq east (1- east))
    (setq left (1+ left))
    ;; stalks are removed from the west set aside in groups of 4, until 4 or fewer remain
    (if (= 0 (% west 4)) (setq group 4) (setq group (% west 4)))
    ;; The remaining stalks are placed between the fingers of left hand.
    (setq left (+ left group))
    ;; stalks are removed from the east set aside in groups of 4, until 4 or fewer remain
    (if (= 0 (% east 4)) (setq group 4) (setq group (% east 4)))
    ;; The remaining stalks are placed between the fingers of left hand.
    (setq left (+ left group))
    ;; The number of stalks between the fingers of the left hand are then counted and noted.
    (message "left: %s" left)
    ;; eight stalks results in the value of 2, four stalks results in the value of 3
    (if (= 8 left)
        (setq result (+ result 2))
      (setq result (+ result 3)))

    ;; These stalks are set aside and the remaining bundle of 32, 36 or 40 stalks is divided
    ;; and counted out in the same manner for a third operation
    (setq yarrow-stalks (- yarrow-stalks left))
    (setq east 0 west 0 left 0 group 0)
    ;; The bundle of the remaining stalks is divided into two bundles
    (dotimes (_ yarrow-stalks)
      (if (= 1 (i-ching-random 2))
          (setq east (1+ east))
        (setq west (1+ west))))
    ;; One stalk is taken from the east bundle and placed between the fingers of the left hand
    (setq east (1- east))
    (setq left (1+ left))
    ;; stalks are removed from the west, set aside in groups of 4 until 4 or fewer remain
    (if (= 0 (% west 4)) (setq group 4) (setq group (% west 4)))
    ;; The remaining stalks are placed between the fingers of left hand.
    (setq left (+ left group))
    ;; stalks are removed from the east set aside in groups of 4, until 4 or fewer remain
    (if (= 0 (% east 4)) (setq group 4) (setq group (% east 4)))
    ;; The remaining stalks are placed between the fingers of left hand.
    (setq left (+ left group))
    ;; The number of stalks between the fingers of the left hand are then counted and noted.
    ;; eight stalks results in the value of 2, four stalks results in the value of 3
    (if (= 8 left)
        (setq result (+ result 2))
      (setq result (+ result 3)))
    ;; three values are now added together to produce a total of 6, 7, 8, or 9
    (pcase result
      (6 '(0 1)) ;; yin changing
      (7 '(1 1)) ;; yang
      (8 '(0 0)) ;; yin
      (9 '(1 0)) ;; yang changing
      )))


;; interpreting a hexagram

;;;###autoload
(defun i-ching-interpretation (hexagram)
  "Consult the I Ching to show an interpretation of a single HEXAGRAM."
  (interactive)
  (format "%s\n\n%s\n\nJudgment: %s\n\nImage: %s\n\n"
          (i-ching-number-to-hexagram hexagram)
          (i-ching-number-to-description hexagram)
          (i-ching-number-to-judgment hexagram)
          (i-ching-number-to-image hexagram)))

;; querying the I Ching

;;;###autoload
(defun i-ching-query-string (&optional method)
  "Consult the I Ching using a particular METHOD."
  (interactive)
  (let* ((cast (i-ching-cast method))
         (hexagram (string (elt cast 0)))
         (changing (when (= 3 (length cast))
                     (string (elt cast 2))))
         (result
          (if changing
              (format "%s → %s\n\n%s\n\n%s"
                      hexagram changing
                      (i-ching-interpretation
                       (i-ching-hexagram-to-number hexagram))
                      (i-ching-interpretation
                       (i-ching-hexagram-to-number changing)))
            (format "%s"
                    (i-ching-interpretation
                     (i-ching-hexagram-to-number hexagram))))))
    (message "cast: %s\n" cast)
    result))

;;;###autoload
(defun i-ching-query (&optional method)
  "Consult the I Ching using a particular METHOD."
  (interactive)
  (read-string "What is your question? ")
  (let* ((query (i-ching-query-string method))
         (reading-buffer (get-buffer-create "*The I Ching*")))
    (with-current-buffer reading-buffer
      (goto-char (point-min))
      (erase-buffer)
      (text-mode)
      (insert query)
      (goto-char (point-max))
      (insert ""))
    (display-buffer reading-buffer))
  t)


;;;etc

(provide 'i-ching)

;;; i-ching.el ends here
