Qualtrics.SurveyEngine.addOnload(function() {

   // SETUP
   var forceErrorCorrection=1;
   var reverseAnswers=0; // user was instructed to reverse the default
   var practiceStatsTimeMS=1000;
   var interQuestionDelay=250; // or 400
   var endMessage=0;
   var stimuliShowCount=0; // 0 -- show all combinations 2*(Astim + Bstim)
   var leftKeyChar="D";
   var rightKeyChar="K";
   var tooSlowMessageMS=2000; // show "too slow" message if tooSlowMessageMS response time exceeded
   var tooSlowMessageShowTimeMS=600; // show "too slow" message for this amount of time
   var practiceMode=1; // evaluate responses to pass practice (append ,OK if success)
   var practiceSuccessThreasholdCorrect=0.80; // minimum % of correct answers to pass practice
   var practiceSuccessThreasholdMedianMS=2000; // minimum median time to pass practice
   var showPracticeStats=1; // show stats (success rate, median time) at the end of each practice block

   var leftKey=leftKeyChar.charCodeAt(0);
   var rightKey=rightKeyChar.charCodeAt(0);

	//DECLARE INITIAL VARIABLES
	var currentStimulus;
	var end; 
	var image_srcs;
	var images; 
	var loadedImages;
	var input; 
	var upperstim;
   var statusMessage;
	var stimuli = [];
	var note;
	var posstim;
	var negstim;
	var Astim;
	var Bstim;
   var ansleft;
   var ansright;

	//USED FOR ALTERNATING TRIAL FORMAT ONLY
	var tgts;
	var cats;
	var tgtnum = [];
	var catnum = [];
	
	//DEFINE addlines. THIS WILL BE PUT IN FRONT OF WORD STIMULI TO DROP THEM DOWN TO BETTER ALIGN WITH IMAGE CENTERS.
 	var addlines="<br>"; // add more if font smaller (30px + "<br>";)	
	
	// THE FOLLOWING ARE ONLY USED IF FORCED ERROR CORRECTION IS ENABLED
	var fix = 0;
	var error;

   // COUNTERS
   var countCorrect;
   var countTotal;
   var responseList=[];
	
	// CLEAN qID VARIABLE OF CHARACTERS AND SAVE AS NUMERIC FOR LATER USE
	var qID =this.questionId; 
	qID = qID.replace("QID", '');
	qID = parseInt(qID);

	// GRAB INPUITID AS REFERENCE TO QUESTION AND HIDE TEXT BOX
   var InputId = document.getElementById("QR~QID" + qID);
	InputId.style.display="none";

	// HIDE NEXT BUTTON
   if (document.getElementById('NextButton')) document.getElementById('NextButton').style.display="none";
   if (document.getElementById('PrevButton')) document.getElementById('PrevButton').style.display="none";
	this.hideNextButton();
	

	//DECLARE FUNCTIONS

   function setInitParams() {
      // initParams exist
      if (typeof initParams == 'undefined' ) {
         console.log("initParams doesn't exist");
         return;
      }

      console.log("initParams exists");

      if ("forceErrorCorrection" in initParams) {
         forceErrorCorrection=initParams.forceErrorCorrection;
         console.log("set forceErrorCorrection to "+forceErrorCorrection);
      }
      if ("reverseAnswers" in initParams) {
         reverseAnswers=initParams.reverseAnswers;
         console.log("set reverseAnswers to "+reverseAnswers);
      }
      if ("interQuestionDelay" in initParams) {
         interQuestionDelay=initParams.interQuestionDelay;
         console.log("set interQuestionDelay to "+interQuestionDelay);
      }
      if ("stimuliShowCount" in initParams) {
         stimuliShowCount=initParams.stimuliShowCount;
         console.log("set stimuliShowCount to "+stimuliShowCount);
      }
      if ("leftKeyChar" in initParams) {
         leftKeyChar=initParams.leftKeyChar;
         leftKey=leftKeyChar.charCodeAt(0);
         console.log("set leftKeyChar to "+leftKeyChar);
      }
      if ("rightKeyChar" in initParams) {
         rightKeyChar=initParams.rightKeyChar;
         rightKey=rightKeyChar.charCodeAt(0);
         console.log("set rightKeyChar to "+rightKeyChar);
      }
      if ("tooSlowMessageMS" in initParams) {
         tooSlowMessageMS=initParams.tooSlowMessageMS;
         console.log("set tooSlowMessageMS to "+tooSlowMessageMS);
      }
      if ("tooSlowMessageShowTimeMS" in initParams) {
         tooSlowMessageShowTimeMS=initParams.tooSlowMessageShowTimeMS;
         console.log("set tooSlowMessageShowTimeMS to "+tooSlowMessageShowTimeMS);
      }
      if ("practiceSuccessThreasholdCorrect" in initParams) {
         practiceSuccessThreasholdCorrect=initParams.practiceSuccessThreasholdCorrect;
         console.log("set practiceSuccessThreasholdCorrect to "+practiceSuccessThreasholdCorrect);
      }
      if ("practiceSuccessThreasholdMedianMS" in initParams) {
         practiceSuccessThreasholdMedianMS=initParams.practiceSuccessThreasholdMedianMS;
         console.log("set practiceSuccessThreasholdMedianMS to "+practiceSuccessThreasholdMedianMS);
      }
      if ("practiceMode" in initParams) {
         practiceMode=initParams.practiceMode;
         console.log("set practiceMode to "+practiceMode);
      }
      if ("showPracticeStats" in initParams) {
         showPracticeStats=initParams.showPracticeStats;
         console.log("set showPracticeStats to "+showPracticeStats);
      }
      /*
         forceErrorCorrection=("forceErrorCorrection" in initParams)?forceErrorCorrection:initParams.forceErrorCorrection;
         reverseAnswers=("reverseAnswers" in initParams)?reverseAnswers:initParams.reverseAnswers;
         interQuestionDelay=("interQuestionDelay" in initParams)?interQuestionDelay:initParams.interQuestionDelay;
         stimuliShowCount=("stimuliShowCount" in initParams)?stimuliShowCount:initParams.stimuliShowCount;
         leftKeyChar=("leftKeyChar" in initParams)?leftKeyChar:initParams.leftKeyChar;
         rightKeyChar=("rightKeyChar" in initParams)?rightKeyChar:initParams.rightKeyChar;
         tooSlowMessageMS=("tooSlowMessageMS" in initParams)?tooSlowMessageMS:initParams.tooSlowMessageMS;
         tooSlowMessageShowTimeMS=("tooSlowMessageShowTimeMS" in initParams)?tooSlowMessageShowTimeMS:initParams.tooSlowMessageShowTimeMS;
         practiceMode=("practiceMode" in initParams)?practiceMode:initParams.practiceMode;
         */
   }

	//SHUFFLER - RANDOMIZES CONTENTS OF AN ARRAY USING A WELL VALIDATED METHOD
	function shuffle(array) {
			var currentIndex = array.length, temporaryValue, randomIndex ;
			
			// While there remain elements to shuffle...
			while (0 !== currentIndex) {
			
				// Pick a remaining element...
				randomIndex = Math.floor(Math.random() * currentIndex);
				currentIndex -= 1;
			
				// And swap it with the current element.
				temporaryValue = array[currentIndex];
				array[currentIndex] = array[randomIndex];
				array[randomIndex] = temporaryValue;
			}
			
			return array;
		};

   // validate that the current ordering does not violate constraints
   function validateStimuliOrdering(array) {
         if (array.length==1) return 1;

         var index=1;
         while(index<array.length) {
            current = array[index];
            prev = array[index-1];

            // make sure we don't have 
            // # The trials are presented quasirandomly, with the typical constraint that none of the four trial types be presented twice in succession.
            if (current.trialType==prev.trialType) {
               return 0;
            }
            index++;
         }
         return 1;
   }
	
	// FUNCTION 1 - IMAGE LOADER
	/* This function is the first command in the IRAP. It is invoked by code at the bottom of this script 
	and puts all image stimuli in an array called images. If the image_srcs object has no URLs in it, this skips ahead
	to the next portion of the IRAP without loading any images. If errors are encountered, the IRAP block is skipped. */
	function loadImages (image_srcs) {
		var src, _i, _len;
		
		//If no images specified, skip this step
		if(	image_srcs.length == 0) {return imagesLoaded();}
		
		images = [];
		loadedImages = 0;
				
		for (_i = 0, _len = image_srcs.length; _i < _len; _i++) {
			src = image_srcs[_i];
			images.push(new Image);

			images[images.length - 1].src = src;
			
			images[images.length - 1].onerror = function() {
				alert("Your web browser encountered an issue running this portion of the study. You will be skipped ahead. You may have to click through this message several times.");
            if (document.getElementById('NextButton')) document.getElementById('NextButton').click();
			};
			
			images[images.length - 1].onload = function() {
				loadedImages++;
				if (loadedImages = images.length) return imagesLoaded();
			};

		}
		return images;
	};
		
	
	// FUNCTION 2 - IMAGES LOADED
	/* Runs when all image loader is finished, starts the keypress listener. 
	For a forced-error-correction IRAP, the last line should return keyCheckForcedError instead of keyCheck*/
	function imagesLoaded() {
		document.getElementById('loading').style.display = 'none';
		document.getElementById('instructions').style.display = 'block';

		if (forceErrorCorrection==0) { return document.addEventListener('keyup', keyCheck, false); }
		else { return document.addEventListener('keyup', keyCheckForcedError, false); }
	};
	

	// FUNCTION 3 - START FUNCTION
	/* Runs when the spacebar is pressed after the keypress listener has begun. Does initial houskeeping (grabs HTML content
	such as message, error, etc. and makes it so we can write to them). It sets as 'input' the contents of the question text box,
	so we can write data to the question by editing that value. It also sets instructions to null. It then shuffles stimuli and
	starts the first trial. */
	 function start() {
        console.log("start");

       endMessage=0;

      //BUILD TRIALS

      // FEATURE: 
      // # The positioning of the two response options is also quasirandom in that typically they cannot appear in the same left–right position three times in succession.

      var keyAtoPosStim;
      var keyAtoNegStim;
      var keyBtoPosStim;
      var keyBtoNegStim;

      if (reverseAnswers==0) {
         keyAtoPosStim=leftKey; 
         keyAtoNegStim=rightKey; 
         keyBtoPosStim=rightKey;
         keyBtoNegStim=leftKey;
      } else {
         keyAtoPosStim=rightKey; 
         keyAtoNegStim=leftKey; 
         keyBtoPosStim=leftKey;
         keyBtoNegStim=rightKey;
      }

      stimBuilder(stimuli, Astim, posstim[0].stimulus, keyAtoPosStim, 1 );
      stimBuilder(stimuli, Bstim, posstim[0].stimulus, keyBtoPosStim, 2 );
      stimBuilder(stimuli, Astim, negstim[0].stimulus, keyAtoNegStim, 3 );
      stimBuilder(stimuli, Bstim, negstim[0].stimulus, keyBtoNegStim, 4 );

      while(1) {
         shuffle(stimuli);
         shuffle(stimuli);
         shuffle(stimuli);
         if (validateStimuliOrdering(stimuli)==0) continue;
	      break;
      }

      if (stimuliShowCount != 0 && stimuliShowCount < stimuli.length) {
         stimuli.length = stimuliShowCount; // trim the length of the stimuli
      }

		upperstim = document.getElementById("upperstim");
		lowerstim = document.getElementById("lowerstim");
		statusMessage = document.getElementById("statusMessage");
		directionMessage = document.getElementById("directionMessage");
		helperMessage = document.getElementById("helperMessage");
		error = document.getElementById("error"); //USED ONLY WITH FORCED ERROR CORRECTION
		input = document.getElementById("QR~QID" + qID);
      input.value = ""; // MAKE SURE WE START WITH AN EMPTY OUTPUT
		ansleft = document.getElementById("ansleft");
		ansright = document.getElementById("ansright");

      // SHOW KEY INSTRUCTIONS
      ansleft.style.display="block";
      ansright.style.display="block";

		//MAKE INSTRUCTIONS EMPTY
		instructions.innerHTML = ""; 
      statusMessage.innerHMTL = ""; 
      directionMessage.innerHMTL = ""; 
      helperMessage.innerHMTL = ""; 

		//ADD NOTE BELOW WINDOW
		note = document.getElementById("note");
		note.innerHTML = "";

      // COUNTER
      countCorrect=0;
      countTotal=0;
		return nextQuestion();
	};
	
	
	// FUNCTION 4 - LAUNCHES QUESTION
	/* This function runs on start() and after a new trial begins. If we have not depleted the stimuli object, it grabs (and removes)
	the last trial from the stimuli object and calls it currentStimulus, then proceeds to use it for an IRAP trial. A start time is
	identified and the stimulus is shown (different methods for images or words). If the stimuli object is depleted, appends END
	to data and advances to the next IRAP block. */
	  function nextQuestion() {
		if (stimuli.length !==0) {

			currentStimulus = stimuli.pop();
				
			// SET MESSAGE TO EMPTY
			upperstim.innerHTML = "";
			lowerstim.innerHTML = "";
         directionMessage.innerHTML = "";
         helperMessage.innerHTML = "";

         console.log("trial "+currentStimulus.category+" "+currentStimulus.stimulus);

			// DECLARE START OF CURRENT STIMULUS
			currentStimulus.start = new Date().getTime();

			lowerstim.innerHTML += addlines + currentStimulus.category;
         upperstim.innerHTML += addlines + currentStimulus.stimulus;

         if (reverseAnswers==0) {
            directionMessage.innerHTML = "SAME Rule";
            helperMessage.innerHTML = "Positive emotions = Good<br>Negative emotions = Bad";
         } else {
            directionMessage.innerHTML = "OPPOSITE Rule";
            helperMessage.innerHTML = "Positive emotions = Bad<br>Negative emotions = Good";
         }
				
 			return;

		} else {

         // HIDE KEY INSTRUCTIONS
         ansleft.style.display="none";
         ansright.style.display="none";

         upperstim.innerHTML = "";
         lowerstim.innerHTML = "";
         directionMessage.innerHTML = "";
         helperMessage.innerHTML = "";

         function sortNumber(a,b) { return a - b; }

         responseList.sort(sortNumber);
         medianResponse = responseList[responseList.length/2];
			input.value += "END";
         if (practiceMode==1 && medianResponse <= practiceSuccessThreasholdMedianMS && countCorrect/countTotal >= practiceSuccessThreasholdCorrect) {
			   input.value += ",OK"; // signal to proceed
         }

         console.log("Output="+input.value);
         if (practiceMode == 1 && showPracticeStats == 1) {
            currentStimulus=0;
            endMessage=1;
            setTimeout(function() {
               if (medianResponse <= practiceSuccessThreasholdMedianMS && countCorrect/countTotal >= practiceSuccessThreasholdCorrect) {
                   statusMessage.innerHTML="You’re doing great!";
               } else {
                 if (countCorrect/countTotal >= practiceSuccessThreasholdCorrect) {
                   statusMessage.innerHTML="<br>";
                 } else {
                   statusMessage.innerHTML="Be a little more accurate.<br>";
                 }
                 if (medianResponse <= practiceSuccessThreasholdMedianMS) {
                   statusMessage.innerHTML+="Your speed is great!";
                 } else {
                   statusMessage.innerHTML+="Try to be a little bit faster.";
                 }
               }
               // statusMessage.innerHTML = "Success rate: "+Math.round(100*countCorrect/countTotal)+"%<br>Median response: "+medianResponse+ "ms";
               
               lowerstim.innerHTML = "<p style='font-size:30px'>Press <b>space</b> to continue.</p>";
               /*setTimeout(function() {
                  if (document.getElementById('NextButton')) document.getElementById('NextButton').click();
               }, practiceStatsTimeMS);
               */
            }, tooSlowMessageShowTimeMS);
         } else {
            if (forceErrorCorrection==0) { document.removeEventListener('keyup', keyCheck, false); }
            else { document.removeEventListener('keyup', keyCheckForcedError, false); }
            if (document.getElementById('NextButton')) document.getElementById('NextButton').click();
         }
         // setTimeout(function() {statusMessage.innerHTML="";}, 1000);

			//WHEN STIMULI HAS NO TRIALS REMAINING, APPEND END, DISABLE KEY LISTENER, AND CLICK NEXT BUTTON
			/* IMPORTANT: In Qualtrics, the keypress listener stays active from one page to the next and 
			must therefore be disabled before continuing. */

		}
	};
	 
	
	// FUNCTION 5 - KEYPRESS LISTENER FOR STANDARD ERROR MODE
	/* This function grabs whatever key was pressed and saves it as keyCode. Depending on what was pressed, it will
	start the IRAP, write data, display next trials, etc. Note that errors are handled by swapping the message out 
	for a red X (the error message below the stimulus is NOT used). Note also that an alternative version of this
	function is defined below but only ONE will be called in a given IRAP.*/
	 function keyCheck(e) {
		var keyCode;
			
		/* depending on how this was triggered, grab keycode and save as keyCode. May need upgrading as web 
		standards improve for keypress handling. */ 
		if (window.event) {
			keyCode = event.keyCode;
		} else {
			keyCode = e.keyCode;
		}
			
		// IF NO CURRENT STIMULUS (ONLY HAPPENS PRIOR TO START OF IRAP), SPACEBAR CAN START IRAP
       // OR AT THE END OF PRACTICE
		if (!currentStimulus) {
			if (keyCode === 32) {
            console.log("space and endMessage="+endMessage);
            if (endMessage==1) {
               document.removeEventListener('keyup', keyCheck, false); 
               if (document.getElementById('NextButton')) document.getElementById('NextButton').click();
            } else {
				  start();
            }
			}
			return;
		}
	
		// NEXT, END FUNCTION IF NOT E OR I KEYS. IF CONTINUING AFTER THIS, MUST BE E OR I KEYPRESS.
		if (!(keyCode === leftKey || keyCode === rightKey)) return;
			
		// ADD END PROPRETY TO CURRENTSTIM
		currentStimulus.end = new Date().getTime();
			
		// CALCULATE DIFFERENCE AND SAVE
		currentStimulus.reactionTime = currentStimulus.end - currentStimulus.start;
      responseList.push(currentStimulus.reactionTime);

		//IF THE KEYCODE MATCHES THE CORRECT PART OF CURRENT STIMULUS, WRITE TO DATA AND DO OTHER STEPS
		//TRADITIONAL ERROR MODE
		if (keyCode === currentStimulus.correct) {				
			input.value += currentStimulus.trialType + "T" + currentStimulus.index + "C" + currentStimulus.reactionTime + ",";	
			upperstim.innerHTML = "<br><br><br>";
			currentStimulus = null;
         countCorrect++;
         countTotal++;
         if (tooSlowMessageMS > 0 && currentStimulus.reactionTime>=tooSlowMessageMS) {
            // statusMessage.innerHTML = "too slow";
			   error.innerHTML = "too slow";
            setTimeout(function() {error.innerHTML="";statusMessage.innerHTML="";}, tooSlowMessageShowTimeMS);
         } else {
			   // error.innerHTML = "";
         }
			return setTimeout(function() {return nextQuestion(); }, interQuestionDelay);
	
		} else {
			input.value += currentStimulus.trialType + "T" + currentStimulus.index + "X" + currentStimulus.reactionTime + ",";
			upperstim.innerHTML = "<br><br><br>+<b style='color:red;font-size:80px'><br><br>X</b>";
			currentStimulus = null;
         countTotal++;
         if (tooSlowMessageMS > 0 && currentStimulus.reactionTime>=tooSlowMessageMS) {
            // statusMessage.innerHTML = "too slow";
			   error.innerHTML = "too slow";
            setTimeout(function() {error.innerHTML="";statusMessage.innerHTML="";}, tooSlowMessageShowTimeMS);
         } else {
			   // error.innerHTML = "";
         }
			setTimeout(function() {return upperstim.innerHTML = "<br><br><br>+";}, 250);
			return setTimeout(function() {return nextQuestion();}, interQuestionDelay);
		}
	};
	
	
	// FUNCTION 5 - KEYPRESS LISTENER FOR FORCED ERROR CORRECTION MODE
	/* This is an alternate form of same function, but it (1) only writes data when a correct response is entered (2) 
	on errors, displays a red X that remains until the correct response is entered, and (3) scores as an error if the 
	initial responses was incorrect (Greenwald et al., 2003). This should be scored without an error penalty as 
	correcting the response naturally builds in an error.  */
	
	function keyCheckForcedError(e) {
		var keyCode;
			
		/* depending on how this was triggered, grab keycode and save as keyCode. May need upgrading as web 
		standards improve for keypress handling. */ 
		if (window.event) {
			keyCode = event.keyCode;
		} else {
			keyCode = e.keyCode;
		}
			
		// IF NO CURRENT STIMULUS (ONLY HAPPENS PRIOR TO START OF IRAP, OR AFTER END MESSAGE STATS), SPACEBAR CAN START IRAP
		if (!currentStimulus) {
			if (keyCode === 32) {
            if (endMessage==1) {
               document.removeEventListener('keyup', keyCheckForcedError, false); 
               if (document.getElementById('NextButton')) document.getElementById('NextButton').click();
            } else {
				  start();
            }
			}
			return;
		}
	
		// NEXT, END FUNCTION IF NOT E OR I KEYS. IF CONTINUING AFTER THIS, MUST BE E OR I KEYPRESS.
		if (!(keyCode === leftKey || keyCode === rightKey)) return;
			
		// note - do NOT grab timing here as it may be an error response
 
		//IF THE KEYCODE MATCHES THE CORRECT PART OF CURRENT STIMULUS, WRITE TO DATA AND DO OTHER STEPS
		//FORCED ERROR CORRECTION MODE.
		if (keyCode === currentStimulus.correct) {				

			// Score the timing and save it
			currentStimulus.end = new Date().getTime();
			currentStimulus.reactionTime = currentStimulus.end - currentStimulus.start;
         responseList.push(currentStimulus.reactionTime);

			if (fix==0){
				input.value += currentStimulus.trialType + "T" + currentStimulus.index + "C" + currentStimulus.reactionTime + ",";	
            countCorrect++;
            countTotal++;
			}

			if (fix==1){
				input.value += currentStimulus.trialType + "T" + currentStimulus.index + "X" + currentStimulus.reactionTime + ",";	// score as error if we had to correct
            countTotal++;
			}

         if (tooSlowMessageMS > 0 && currentStimulus.reactionTime>=tooSlowMessageMS) {
            // statusMessage.innerHTML = "too slow";
			   error.innerHTML = "<div class='tooSlow'>too slow</div>";
            setTimeout(function() {error.innerHTML="";statusMessage.innerHTML="";}, tooSlowMessageShowTimeMS);
         }
			// upperstim.innerHTML = "<br><br><br>+";
			fix=0;
			currentStimulus = null;
			// error.innerHTML = "";
			return setTimeout(function() {return nextQuestion(); }, interQuestionDelay);
	
		} else {
			error.innerHTML = "X";
			fix=1;
			return;
		}
	};


	//FUNCTION 6 - TAKES CONTENTS FROM A STIMULI POOL AND PLACES INTO STIMULI OBJECT
	function stimBuilder(destination, array, category, correct, trialType){
      for(i=0;i<array.length;i++) {
         destination.push({stimulus: array[i].stimulus, 
                          index: array[i].index,
                          correct: correct, 
                          category: category,
                          trialType: trialType
         });
		}
	}


   // After this define data and start
   // Astim = 
   // Bstim = 
   // image_srcs =
   // initParams = 
   // setInitParams
	

	//  IRAP CONTENTS 
	
	//IMAGE URLS
	/* Consists of all pos, neg, A, and B images (in that order). */
 image_srcs = [];



	// THIS IS WHAT STARTS THE IAT
	/* This line of code triggers the IAT by running the first function. Note that the code skips image loading if image_srcs 
	is empty. Note that this must come before stimuli in order that images[] references are defined.  */
	images = loadImages(image_srcs);

	// STIMULI POOLS
/* GENERATED */
initParams = { forceErrorCorrection:1, reverseAnswers:0, interQuestionDelay:250, stimuliShowCount:0, leftKeyChar:"D", rightKeyChar:"K", tooSlowMessageMS:2000, tooSlowMessageShowTimeMS:600, practiceMode:0 };
   /* GENERATED */
   posstim = [
      {stimulus: "<b style='color:green'>good</b>", correct:"NA", index: 1}
   ];

   negstim = [
      {stimulus: "<b style='color:green'>bad</b>", correct:"NA", index: 2}
   ];
Astim = [
{stimulus: "<b style='color:black'>calm</b>", correct:69, index: 3},
{stimulus: "<b style='color:black'>relaxed</b>", correct:69, index: 4},
{stimulus: "<b style='color:black'>comfortable</b>", correct:69, index: 5},
{stimulus: "<b style='color:black'>content</b>", correct:69, index: 6},
];
Bstim = [
{stimulus: "<b style='color:black'>nervous</b>", correct:73, index: 7},
{stimulus: "<b style='color:black'>anxious</b>", correct:73, index: 8},
{stimulus: "<b style='color:black'>afraid</b>", correct:73, index: 9},
];


setInitParams();

});
