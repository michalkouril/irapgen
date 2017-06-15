Qualtrics.SurveyEngine.addOnload(function() {

   // SETUP
   var forceErrorCorrection=1;
   var reverseAnswers=0; // user was instructed to reverse the default
   var practiceStatsTimeMS=1000;
   var interQuestionDelay=250; // or 400
   var endMessage=0;
   // FIXME: number of stimuli * 2 (number of categories)
   var stimuliShowCount=12; // divisible by 4 // 48;
   var leftKeyChar="D";
   var rightKeyChar="K";
   var tooSlowMessageMS=2000; // show "too slow" message if tooSlowMessageMS response time exceeded
   var tooSlowMessageShowTimeMS=600; // show "too slow" message for this amount of time
   var practiceMode=1; // show stats after each practice

   var leftKey=leftKeyChar.charCodeAt(0);
   var rightKey=rightKeyChar.charCodeAt(0);

	//DECLARE INITIAL VARIABLES
	var currentStimulus;
	var end; 
	var image_srcs;
	var images; 
	var loadedImages;
	var input; 
	var message;
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
 	var addlines="<br><br><br>";	
	
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
      if (typeof initParams == 'undefined' ) { // || !( initParams instanceof Array ) ) {
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
      if ("practiceMode" in initParams) {
         practiceMode=initParams.practiceMode;
         console.log("set practiceMode to "+practiceMode);
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
   }
	
	// FUNCTION 1 - IMAGE LOADER
	/* This function is the first command in the IAT. It is invoked by code at the bottom of this script 
	and puts all image stimuli in an array called images. If the image_srcs object has no URLs in it, this skips ahead
	to the next portion of the IAT without loading any images. If errors are encountered, the IAT block is skipped. */
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
	For a forced-error-correction IAT, the last line should return keyCheckForcedError instead of keyCheck*/
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
       endMessage=0;

      //EMPTY SET OF TRIALS - LOADS FROM POOLS ABOVE
      //BUILD TRIALS
      precreateStimuliArray(stimuliShowCount);

      var cutoffs = [0, stimuli.length/4, stimuli.length/2, 3*stimuli.length/4, stimuli.length];

      // FIXME: make sure 
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

      stimBuilder(Astim, stimuli, cutoffs[0], cutoffs[1], posstim[0].stimulus, keyAtoPosStim, 1 );
      stimBuilder(Astim, stimuli, cutoffs[1], cutoffs[2], negstim[0].stimulus, keyAtoNegStim, 2 );
      stimBuilder(Bstim, stimuli, cutoffs[2], cutoffs[3], posstim[0].stimulus, keyBtoPosStim, 3 );
      stimBuilder(Bstim, stimuli, cutoffs[3], cutoffs[4], negstim[0].stimulus, keyBtoNegStim, 4 );

      shuffle(stimuli);
      while(validateStimuliOrdering(stimuli)==0) { 
         shuffle(stimuli);
      }


		message = document.getElementById("message");
		category = document.getElementById("category");
		statusMessage = document.getElementById("statusMessage");
		error = document.getElementById("error"); //USED ONLY WITH FORCED ERROR CORRECTION
		input = document.getElementById("QR~QID" + qID);
		ansleft = document.getElementById("ansleft");
		ansright = document.getElementById("ansright");

      // SHOW KEY INSTRUCTIONS
      ansleft.style.display="block";
      ansright.style.display="block";

		//MAKE INSTRUCTIONS EMPTY
		instructions.innerHTML = ""; 
      statusMessage.innerHMTL = ""; 

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
	the last trial from the stimuli object and calls it currentStimulus, then proceeds to use it for an IAT trial. A start time is
	identified and the stimulus is shown (different methods for images or words). If the stimuli object is depleted, appends END
	to data and advances to the next IAT block. */
	  function nextQuestion() {
		if (stimuli.length !==0) {

			currentStimulus = stimuli.pop();
				
			// SET MESSAGE TO EMPTY
			message.innerHTML = "";
			category.innerHTML = "";
				
			// DECLARE START OF CURRENT STIMULUS
			currentStimulus.start = new Date().getTime();

			category.innerHTML += addlines + currentStimulus.category;
				
			// FOR IMAGES, USE APPEND CHILD TO DISPLAY. IF NOT, ADD STIMULUS VALUE TO MESSAGE.
			if (typeof currentStimulus.stimulus === 'object') {
 				return message.appendChild(currentStimulus.stimulus);
			} else {
 				return message.innerHTML += addlines + currentStimulus.stimulus;
 			}

		} else {

         // HIDE KEY INSTRUCTIONS
         ansleft.style.display="none";
         ansright.style.display="none";

         message.innerHTML = "";
         category.innerHTML = "";
         responseList.sort();
         medianResponse = responseList[responseList.length/2];
			input.value += "END";
         if (practiceMode==1 && medianResponse<=tooSlowMessageMS && countCorrect/countTotal>=0.80) {
			   input.value += ",OK"; // signal to proceed
         }

         if (practiceMode==1) {
            currentStimulus=0;
            endMessage=1;
            setTimeout(function() {
               statusMessage.innerHTML = "Success rate: "+Math.round(100*countCorrect/countTotal)+"%<br>Median response: "+medianResponse+ "ms";
               message.innerHTML = "Press <b>space</b> to continue.";
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
	start the IAT, write data, display next trials, etc. Note that errors are handled by swapping the message out 
	for a red X (the error message below the stimulus is NOT used). Note also that an alternative version of this
	function is defined below but only ONE will be called in a given IAT.*/
	 function keyCheck(e) {
		var keyCode;
			
		/* depending on how this was triggered, grab keycode and save as keyCode. May need upgrading as web 
		standards improve for keypress handling. */ 
		if (window.event) {
			keyCode = event.keyCode;
		} else {
			keyCode = e.keyCode;
		}
			
		// IF NO CURRENT STIMULUS (ONLY HAPPENS PRIOR TO START OF IAT), SPACEBAR CAN START IAT
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
			input.value += currentStimulus.index + "C" + currentStimulus.reactionTime + ",";	
			message.innerHTML = "<br><br><br>+";
			currentStimulus = null;
         countCorrect++;
         countTotal++;
         if (currentStimulus.reactionTime>=tooSlowMessageMS) {
            statusMessage.innerHTML = "too slow";
			   // error.innerHTML = "too slow";
            setTimeout(function() {statusMessage.innerHTML="";}, tooSlowMessageShowTimeMS);
         } else {
			   // error.innerHTML = "";
         }
			return setTimeout(function() {return nextQuestion(); }, interQuestionDelay);
	
		} else {
			input.value += currentStimulus.index + "X" + currentStimulus.reactionTime + ",";
			message.innerHTML = "<b style='color:red;font-size:80px'><br><br>X</b>";
			currentStimulus = null;
         countTotal++;
         if (currentStimulus.reactionTime>=tooSlowMessageMS) {
            statusMessage.innerHTML = "too slow";
			   // error.innerHTML = "too slow";
            setTimeout(function() {statusMessage.innerHTML="";}, tooSlowMessageShowTimeMS);
         } else {
			   // error.innerHTML = "";
         }
			setTimeout(function() {return message.innerHTML = "<br><br><br>+";}, 250);
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
			
		// IF NO CURRENT STIMULUS (ONLY HAPPENS PRIOR TO START OF IAT), SPACEBAR CAN START IAT
		if (!currentStimulus) {
			if (keyCode === 32) {
            console.log("space and endMessage="+endMessage);
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
				input.value += currentStimulus.index + "C" + currentStimulus.reactionTime + ",";	
            countCorrect++;
            countTotal++;
			}

			if (fix==1){
				input.value += currentStimulus.index + "X" + currentStimulus.reactionTime + ",";	// score as error if we had to correct
            countTotal++;
			}

         if (currentStimulus.reactionTime>=tooSlowMessageMS) {
            statusMessage.innerHTML = "too slow";
			   // error.innerHTML = "too slow";
            setTimeout(function() {statusMessage.innerHTML="";}, tooSlowMessageShowTimeMS);
         }
			// message.innerHTML = "<br><br><br>+";
			fix=0;
			currentStimulus = null;
			error.innerHTML = "";
			return setTimeout(function() {return nextQuestion(); }, interQuestionDelay);
	
		} else {
			error.innerHTML = "X";
			fix=1;
			return;
		}
	};


   function precreateStimuliArray(count) {
      for(var i=0;i<count;i++) {
         stimuli.push({stimulus: "", correct: "", index: "", trialType: 0});
      }
   }
	
	//FUNCTION 6 - TAKES CONTENTS FROM A STIMULI POOL AND PLACES INTO PORTION OF AN OBJECT
	/* This function takes items from a given stimuli pool and places it randomly into portions of a destination object (positions
	between start and end). This is used, for example, to take portions of posstim and put it into a portion of the final 
	stimuli object, or to move contents into intermediate objects that can then be placed (in alternating order) into a
	final stimuli object. */
	
	function stimBuilder(array, destination, start, end, cat, correct, trialType){
		var i = start;
		while(i<end){
			shuffle(array);
			for(var j=0; j<array.length; j++){
				destination[i].stimulus= array[j].stimulus;
				destination[i].index= array[j].index;
				destination[i].correct= correct;
				destination[i].category= cat;
				destination[i].trialType= trialType;
				i++;
				if (i === end){return;}
			}
		}
	}
	
	

	
	//FUNCTION 7 - FOR COMBINED BLOCKS WITH ALTERNATING FORMAT ONLY
	/* For combined blocks with an alternating form, this function is used to transfer stimuli from 
	intermediary pools (cats and tgts) into the final stimuli object in an alternating format */
	function altStimuil(){
		
		//CREATE INDICES OF ALTERNATING NUMBERS TO USE FOR TGT AND CAT TRIALS
		var j = 0;
		for(var i = 1; i < stimuli.length; i += 2) { 
			tgtnum[j] = i; // starts at 1
			catnum[j] = (i-1); // starts at 0
			j++; // represents position in tgt/cat number array
		}
	
		// FOR ALL TARGETS, MOVE CONTENTS FROM TARGET INTO STIMULI USING TGTNUM TO INDEX TRIALS
		for (var i = 0; i < tgts.length; i++){
			var alternating = tgtnum[i];
			stimuli[alternating].stimulus = tgts[i].stimulus;
			stimuli[alternating].correct = tgts[i].correct;
			stimuli[alternating].index = tgts[i].index;
		}
		
		// FOR ALL CATEGORIES, MOVE CONTENTS FROM TARGET INTO STIMULI USING CATNUM TO INDEX TRIALS
		for (var i = 0; i < cats.length; i++){
			var alternating = catnum[i];
			stimuli[alternating].stimulus = cats[i].stimulus;
			stimuli[alternating].correct = cats[i].correct;
			stimuli[alternating].index = cats[i].index;
		}
	}

	

	//  IAT CONTENTS 
	
	//IMAGE URLS
	/* Consists of all pos, neg, A, and B images (in that order). */
