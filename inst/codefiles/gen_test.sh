#!/bin/bash

# FIXME:
# The trials are presented quasirandomly, with the typical constraint that none of the four trial types be presented twice in succession.7 
# The positioning of the two response options is also quasirandom in that typically they cannot appear in the same left–right position three times in succession.8

interQuestionMS=250

# display red X when wrong and wait
force=True

# show response keys = always

# show "too slow" message if response longer than 2000ms
tooSlowMessageMS=2000

# 48 trials per block
trialsPerBlock=48

# practice blocks
# don't record data for practice blocks
# receive feedback on speed and accuracy at the end of each block
minMedianToProceed=2000
minAccuracyToProceed=0.80

# make sure the category alternate
showAlternateCategory=0

# this is setup in Qualtrics flow
# maxPracticeBlocks=6
# minPracticeBlocks=2

## Test blocks ##
# no feedback at the end of each block
# this is setup in Qualtrics flow
# totalTestBlocks=6

# show all stimuli
stimuliShowCount=0

tgtA="calm relaxed comfortable content"
tgtB="nervous anxious afraid"

catA=good
catB=bad

cat - > codePARAMS_practice_pos.js <<EOF
/* GENERATED */
initParams = { forceErrorCorrection:1, reverseAnswers:0, interQuestionDelay:$interQuestionMS, stimuliShowCount:$stimuliShowCount, leftKeyChar:"D", rightKeyChar:"K", tooSlowMessageMS:2000, tooSlowMessageShowTimeMS:600, practiceMode:1, practiceSuccessThreasholdCorrect:0.80, practiceSuccessThreasholdMedianMS:2000, showPracticeStats:1 };
EOF

cat - > codePARAMS_practice_neg.js <<EOF
/* GENERATED */
initParams = { forceErrorCorrection:1, reverseAnswers:1, interQuestionDelay:$interQuestionMS, stimuliShowCount:$stimuliShowCount, leftKeyChar:"D", rightKeyChar:"K", tooSlowMessageMS:2000, tooSlowMessageShowTimeMS:600, practiceMode:1, practiceSuccessThreasholdCorrect:0.80, practiceSuccessThreasholdMedianMS:2000, showPracticeStats:1  };
EOF

cat - > codePARAMS_test_pos.js <<EOF
/* GENERATED */
initParams = { forceErrorCorrection:1, reverseAnswers:0, interQuestionDelay:$interQuestionMS, stimuliShowCount:$stimuliShowCount, leftKeyChar:"D", rightKeyChar:"K", tooSlowMessageMS:2000, tooSlowMessageShowTimeMS:600, practiceMode:0 };
EOF

cat - > codePARAMS_test_neg.js <<EOF
/* GENERATED */
initParams = { forceErrorCorrection:1, reverseAnswers:1, interQuestionDelay:$interQuestionMS, stimuliShowCount:$stimuliShowCount, leftKeyChar:"D", rightKeyChar:"K", tooSlowMessageMS:2000, tooSlowMessageShowTimeMS:600, practiceMode:0 };
EOF

(
cat - <<EOF
   /* GENERATED */
   posstim = [
      {stimulus: "<b style='color:green'>$catA</b>", correct:"NA", index: 1}
   ];

   negstim = [
      {stimulus: "<b style='color:green'>$catB</b>", correct:"NA", index: 2}
   ];
EOF

index=3;

echo "Astim = ["

for word in $tgtA ;
do
  echo "{stimulus: \"<b style='color:black'>$word</b>\", correct:69, index: $index},"
  index=$((index+1))
done

echo "];"

echo "Bstim = ["

for word in $tgtB ;
do
  echo "{stimulus: \"<b style='color:black'>$word</b>\", correct:73, index: $index},"
  index=$((index+1))
done

echo "];"

)> codeSTIM.js

cat codeA.js codeIMG.txt codeB.txt codePARAMS_practice_pos.js codeSTIM.js codeC.txt > js_pp.js # practice positive
cat codeA.js codeIMG.txt codeB.txt codePARAMS_practice_neg.js codeSTIM.js codeC.txt > js_pn.js # practice negative
cat codeA.js codeIMG.txt codeB.txt codePARAMS_test_pos.js codeSTIM.js codeC.txt > js_tp.js # test positive
cat codeA.js codeIMG.txt codeB.txt codePARAMS_test_neg.js codeSTIM.js codeC.txt > js_tn.js # test negative


# qualtrics needs:
# js_pp.js html_template_practice.html
# js_pn.js html_template_practice.html
# js_tp.js html_template_test.html
# js_tn.js html_template_test.html

sed '/HTML/ r html_template_practice.html' test.html | sed '/JS/ r js_pp.js'  > html_pp.html
sed '/HTML/ r html_template_practice.html' test.html | sed '/JS/ r js_pn.js'  > html_pn.html
sed '/HTML/ r html_template_test.html' test.html | sed '/JS/ r js_tp.js'  > html_tp.html
sed '/HTML/ r html_template_test.html' test.html | sed '/JS/ r js_tn.js'  > html_tn.html
