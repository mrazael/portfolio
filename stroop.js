///ONLINE EXPERIMENTS FOR LANGUAGE SCIENTISTS (SEMESTER 1, 2023)
///STUDENT NUMBER @@@@@@@@///

//-----------------------------------------------------------------------------------------------//
/// SAVE DATA AND INITIALISE///

var jsPsych = initJsPsych({
    on_finish: function () {
    var subject_id = jsPsych.randomization.randomID(15)
    jsPsych.data.addProperties({subject: subject_id});
// Get only the subject, rt, and condition entries for each trial.
    var dc1 = jsPsych.data.get().filter({trial_type: 'image-keyboard-response'});
    var dc2 = jsPsych.data.get().filter({trial_type: 'html-audio-response'});
    var dc3 = jsPsych.data.get().filter({trial_type: 'audio-button-response'});
    var data = dc1.join(dc2);
    var data = data.join(dc3)
    var data = data.filterColumns(['subject', 'trial_index', 'stimulus', 'text', 'colour', 'shape', 'response', 'rt']);
    var data_as_csv = data.csv(); //convert to csv format
    save_data("stroop_data.csv", data_as_csv); //save it
//    save_data("stroop_data.csv", dc3);
    ///FOR TESTING ONLY, DUMP ALL DATA ON SCREEN ON FINISH///
//    jsPsych.data.displayData("csv"); //and also dump the data to screen
  },
});

function save_data(name, data_in) {
  var url = "save_data.php";
  var data_to_send = { filename: name, filedata: data_in };
  fetch(url, {
    method: "POST",
    body: JSON.stringify(data_to_send),
    headers: new Headers({
      "Content-Type": "application/json",
    }),
  });
}

//-----------------------------------------------------------------------------------------------//

///PRELOAD DATA///

var preload = {
  type: jsPsychPreload,
  auto_preload: true,
};

var browsercheck = {
  type: jsPsychBrowserCheck,
  inclusion_function: (data) => {
    return ['chrome', 'firefox'].includes(data.browser);
  },
  exclusion_message: `<p>You must use Chrome or Firefox to complete this experiment.</p>`
};

// reCAPTCHA object
//var recaptcha = {
//    type: jsPsychExternalHtml,
//    url: "recaptcha.html",
//    cont_btn: "submit_button",
//    execute_script: true
//};


//var mobilecheck = {
//  type: jsPsychBrowserCheck,
//    inclusion_function: (data) => {
//    return data.mobile === false;
//  },
//  exclusion_message: `<p>You must use a desktop/laptop computer to participate in this experiment.</p>`
//};


//-----------------------------------------------------------------------------------------------//

///VARIABLES FOR ALL THE TASKS///

var audiodataB = ["champ", "gnome", "quip", "sign", "thrift"]
//var audiodataC = ["p3bald", "p3lymph", "p3rhyme", "p3spread", "p3wand"] /// NOT USED IN THIS DEMO
var experimentB = audiodataB.map(v => "audiodata/AppendixB/" + v + ".wav")
//var experimentC = audiodataC.map(v => "audiodata/AppendixC/" + v + ".wav") /// NOT USED IN THIS DEMO
var text = ['blue', 'violet', 'red', 'orange'];
var colour = ['#0072c3', '#cc79a7', '#da1e28', '#ff832b'];
var exp1shapes = ['square'];
//var exp2shapes = ['square', 'circle']; /// NOT USED IN THIS DEMO
var experiment1 = text.flatMap(d => exp1shapes.map(v => "stroop_shapes/" + d + v + ".png"));
//var experiment2 = text.flatMap(d => exp2shapes.map(v => "stroop_shapes/" + d + v + ".png")); /// NOT USED IN THIS DEMO
var i_trials = 16;
var factors = {colour: colour,
               text: text,
               shape: experiment1
               };
var values = jsPsych.randomization.factorial(factors, 1);


//-----------------------------------------------------------------------------------------------//

/// WELCOME SCREEN AND INSTRUCTIONS FOR THE DIFFERENT TASKS ///

var check = {
  type: jsPsychSurveyMultiSelect,
  questions: [
    {
      prompt: `<div class = "container">Online Experiments for Language Scientists: Final assessment</div><br>
            <div class = "content">Welcome to the study. Here you will be presented with few introductory slides before we begin with the tasks at hand.
            This study must be completed on a desktop or a laptop computer. To continue, check 'I am human' box.</div>`,
      options: ["I am human"],
      horizontal: true,
      required: true,
      }
    ],
};

var welcome = {
    type: jsPsychInstructions,
    pages: [`<div class = "content">We are interested in how different psychological tasks can be used as a proxy measure for reading disabilities such as dyslexia.
            This study is in two Parts. To complete this study all you need is a working microphone.<br><br>
            Part 1 includes three language related tasks designed to measure your reading ability. In Part 2, you will engage with different pictures and task-related distractions
            in a reaction time task. The theory is that those with better reading ability face less difficulties in the reaction task.
            Please note that this study does not provide and cannot be used as a diagnosis for any learning disabilities. If you are concerned you might suffer from dyslexia or other form of
            learning impairment, please head to (LOCALISED SITE WOULD BE PROVIDED HERE).</div>`,
            `<img style="margin:10px; padding:10px"; src="images/lock.png" alt="secure"><br>
            <div class = "content">On the next page, you will be asked to allow access to your microphone.
            Check that you do not have Adblock or any other browser plugins interfering with the access request. Make sure
            the site has a secure connection, depicted by the lock icon on the above picture. If not, and your address bar looks like the picture below, head to
            https://jspsychlearning.ppls.ed.ac.uk/~s2023370/stroop_task/stroop.html in order to continue with the study.<br></div>
            <img style="margin:10px; padding:10px"; src="images/nolock.png" alt="not secure">`],
    button_label_next: "Next page",
    button_label_previous: "Previous page",
    show_clickable_nav: true
};


/// ACCESS TO MICROPHONE///
var microphone = {
    type: jsPsychInitializeMicrophone
};
//-----------------------------------------------------------------------------------------------//

/// PART 1 - READING ///
var instructions1A = {
    type: jsPsychInstructions,
    pages: [`<div class = "container">PART 1</div><br>
    <div class = "content">This part has three stages, and it is designed to measure reading ability as well as phonological awareness.
    In Part A, we will record audio of you repeating words which will appear on the screen;
    in Part B, you will hear recordings of words, and you will be asked to write down what those words are; in Part C, you will be asked to write down
    words in a phonemic manner. All instructions will be repeated before each part begins. Press 'Next page' when you are ready to continue to Part A.</div>`,
    `<div class = "container">PART A</div><br>
    <div class = "content">We will now begin Part A. On the next page, nine words will appear on your screen simultaneously.
    When the words appear, you will have four seconds to say out loud as many words as you can.
    Do not worry if you are unable to say every word. The task has been designed so that there are more words than what it is possible to say in the given timeframe.</div><br>
    When you are ready, press "Next page" to begin Part A.<br>`],
    button_label_next: "Next page",
    button_label_previous: "Previous page",
    show_clickable_nav: true
};


/// PART 1 - SPELLING ///
var instructions1B = {
    type: jsPsychInstructions,
    pages: [`<div class = "container">PART B</div><br>
    <div class = "content">That concludes Part A. We will now begin Part B. You will be presented with audio of five words one at a time. This task has no time limit, and you are allowed to
    repeat the audio if necessary. After hearing the word you will then be asked to write down what that word is.</div><br>
    When you are ready, press "Next page" to begin Part B.<br>`],
    button_label_next: "Next page",
    button_label_previous: "Previous page",
    show_clickable_nav: true
};

/// PART 1 - PHONEMIC SPELLING ///
var instructions1C = {
    type: jsPsychInstructions,
    pages: [`<div class = "container">PART C</div><br>
    <div class = "content">That concludes Part B. We will now begin Part C, which is the last part of the reading task. The task has no time limit. You may repeat the audio if necessary.<br><br>
    You will hear two of the previously heard words again. This time, we would like you to give a spelling for each word so that someone who has never seen the word could still pronounce it properly.
    For example, if you heard the word ‘yacht’ you could write down ‘Y O T’.</div><br>
    When you are ready, press "Next page" to begin Part C.<br>`],
    button_label_next: "Next page",
    button_label_previous: "Previous page",
    show_clickable_nav: true
};

///PART 2 - STROOP TASK///
var instructions2 = {
    type: jsPsychInstructions,
    pages: [`That concludes Part 1.`,
            `<div class = "content">We will now begin Part 2. This experiment is known as <strong>The Stroop Task</strong>. You will be shown sixteen images of geometric shapes and a text
            describing the color of that shape. Your task is to press either <strong>F</strong> or <strong>J</strong> on your keyboard depending whether
            the colour of the shape and the accompanying text below are a match.</div>`,

            `<div id="container">
                <div class="images">
                    <img src="stroop_shapes/bluesquare.png"/>
                    <div class = "container"><p class="center", style="color:#0072c3"><strong>BLUE</strong></p></div>
                </div>
                <div class="images">
                    <img src="stroop_shapes/redsquare.png"/>
                    <div class = "container"><p class="center", style="color:#ff832b"><strong>RED</strong></p></div>
                </div>
            </div><br>
            <div class = "content">In both of these cases, you should press <strong>F</strong>, as the word below the shape describes its colour, even if the colour of the text is not a match.</div>`,
            `<div class = "content">In these types of cases you should press 'J', as the word below the shape does not describe its colour.</div>
            <div id="container">
            <div class="images">
                    <img src="stroop_shapes/violetsquare.png"/>
                    <div class = "container"><p class="center", style="color:#cc79a7"><strong>ORANGE</strong></p></div>
                </div>
                <div class="images">
                    <img src="stroop_shapes/orangesquare.png"/>
                    <div class = "container"><p class="center", style="color:#ff832b"><strong>BLUE</strong></p></div>
                </div>
                <div class="images">
                    <img src="stroop_shapes/bluesquare.png"/>
                    <div class = "container"><p class="center", style="color:#da1e28"><strong>ORANGE</strong></p></div>
                </div>
            </div>`,
    ],
    button_label_next: "Next page",
    button_label_previous: "Previous page",
    show_clickable_nav: true
};


///FINAL SCREEN///
var final_screen = {
    type: jsPsychHtmlButtonResponse,
    stimulus: `Finished! Thank you for taking part in the experiment.<br>
            In a live version, a completion code would be given here, enabling participants to claim payment.<br>
            If you have any questions about the experiments, do not hesitate to contact s2023370@ed.ac.uk.`,
    choices: ['Thank you and good bye!'],
};

//-----------------------------------------------------------------------------------------------//

///PART 1 TRIALS///

var fixation1 = {
    type: jsPsychHtmlKeyboardResponse,
    stimulus: '+',
    trial_duration: 2000,
    response_ends_trial: false
};

var audiotrial = {
    type: jsPsychHtmlAudioResponse,
    stimulus: `<div class = "container space-around">
                <div class = "container space-between">bed brandy caddy</div><br>
                <div class = "container space-between">blood depot good</div><br>
                <div class = "container space-between">aspy baft grenty</div>
                </div>`,
    recording_duration: 4000,
    show_done_button: false,
    data: {text: "trial words go here"}
    };

var audioquestion = {
    type: jsPsychSurveyText,
    questions: [
    {prompt: 'Write the word you just heard', rows: 1}
  ],
}

var trials = [browsercheck, preload, check, welcome, microphone, instructions1A, fixation1, audiotrial]

trials.push(instructions1B)

for (i=0; i<experimentB.length; i++) {

    var audiorepeat = {
        timeline: [{
        type: jsPsychAudioButtonResponse,
        stimulus: experimentB[i],
        choices: ['Play again','Continue'],
        }],
        loop_function: function(data){
        if(data.values()[0].response == 0){
        return true; // loop again!
        } else {
        return false; // continue
      };
    },
  };
  trials.push(audiorepeat);
  trials.push(audioquestion);
};

trials.push(instructions1C)

for (i=0; i<2; i++) {

    var audiorepeat = {
        timeline: [{
        type: jsPsychAudioButtonResponse,
        stimulus: experimentB[i],
        choices: ['Play again','Continue'],
        }],
        loop_function: function(data){
        if(data.values()[0].response == 0){
        return true; // loop again!
        } else {
        return false; // continue
      };
    },
  };
  trials.push(audiorepeat);
  trials.push(audioquestion);
};

//-----------------------------------------------------------------------------------------------//

///PART 2 - STROOP TASK TRIALS///



var pressF = {
    type: jsPsychHtmlKeyboardResponse,
    stimulus: 'Please place your fingers on the letters "F" and "J" on your keyboard.<br>Press "F" to continue.',
    choices: ['f'],
    };

var pressJ = {
    type: jsPsychHtmlKeyboardResponse,
    stimulus: 'Press "J" to begin the experiment.',
    choices: ['j'],
    };

trials.push(instructions2)

var fixation2 = {
    type: jsPsychHtmlKeyboardResponse,
    stimulus: '+',
    trial_duration: 750,
    response_ends_trial: false
};


trials.push(pressF);
trials.push(pressJ);

for (var i=0; i<i_trials; i++) {

    var trial = {
        type: jsPsychImageKeyboardResponse,
        stimulus: values[i].shape,
        choices: ['f','j'],
        prompt: '<div class = second><p style="color: '+values[i].colour+'">'+values[i].text+'</p></div>',
        data: values[i],
    };
    trials.push(fixation2);
    trials.push(trial);

};
//-----------------------------------------------------------------------------------------------//

trials.push(final_screen)

jsPsych.run(trials);
