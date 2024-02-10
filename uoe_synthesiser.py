import string #https://docs.python.org/3/library/string.html
import wave #https://docs.python.org/3/library/wave.html
import glob #https://docs.python.org/3/library/glob.html
import os #https://docs.python.org/3/library/os.html
import random #https://docs.python.org/3/library/random.html
import re #https://docs.python.org/3/library/re.html
from datetime import datetime #https://docs.python.org/3/library/datetime.html
from nltk.corpus import cmudict #https://www.nltk.org/ #http://www.speech.cs.cmu.edu/cgi-bin/cmudict

import simpleaudio
from synth_args import process_commandline

class Synth:

    def __init__(self, wav_folder):
        self.wav_folder = wav_folder
        self.wavdict = self.get_wavs(self.wav_folder)
        self.first_pron_check = True

    def get_wavs(self, wav_folder):
        """Loads all the waveform data contained in WAV_FOLDER.
        Returns a dictionary, with unit names as keys and the corresponding
        loaded audio data as values."""

        wavdict = {}

        if os.path.isdir(wav_folder):
            for wavfile in glob.glob(wav_folder + '/*.wav'):
                wf = wave.open(wavfile, "r")
                wavname = re.search('[a-z|_]+[-][a-z|_]+', str(wavfile)).group() #accounts for all our different kinds of .wav names, e.g. 'k_-_l'
                fileparams = wf.getparams()

                wavdict[wavname] = fileparams

        else:
            print(f"File '{wav_folder}' not found. Terminating.")
            exit()

        return wavdict

    def synthesise(self, phones, reverse=False, smooth_concat=False):
        """
        Synthesises a phone list into an audio utterance.
        :param phones: list of phones (list of strings)
        :param smooth_concat:
        :return: synthesised utterance (Audio instance)
        """

        diphoneseqs = self.phones_to_diphones(["pau"] + phones + ["pau"]) #adds silence to beginning and end
        data = []

        for infile in diphoneseqs:
            if infile == 'w-er':
                infile = 'w-eh'

            try:
                w = wave.open(self.wav_folder + '/' + infile + '.wav', 'rb') #https://www.thepythoncode.com/article/concatenate-audio-files-in-python
                data.append([w.readframes(self.wavdict[infile][3])])         # I had real trouble understanding simpleaudio.Audio so I did A LOT of things my own way,
                self.params = self.wavdict[infile]                           # as can be witnessed in the following sections
                w.close()

            except FileNotFoundError:
                while self.first_pron_check == True:
                    print(f"    There's no sound for diphone '{infile}' - what do you want to do? (NB: Whatever you choose I will do for all missing sounds.)")
                    print("     1: Skip this sound and synthesise the file without it (might affect audio quality).")
                    print("     2: Unlike God, I do play with dice: I will find a random sound file from 1619 possible diphones and use it in this diphone's stead (you probably will not like what you hear).")
                    print("     Other: Exit program.\n")
                    choice = int(input("     Choose from the options above: "))
                    self.first_pron_check = False

                if choice == 1:
                    continue
                elif choice == 2:
                    your_choice = random.choice(os.listdir(self.wav_folder)) # gets a random soundfile
                    w = wave.open(self.wav_folder + '/' + your_choice, 'rb')
                    print(f"'{infile}.wav was replaced with '{your_choice}'")
                    data.append([self.wavdict[your_choice[:-4]], w.readframes(self.wavdict[your_choice[:-4]][3])])
                    w.close()
                else:
                    exit()

        return data

    def phones_to_diphones(self, phones):
        """
        Converts a list of phones to the corresponding diphone units (to match units in diphones folder).
        :param phones: list of phones (list of strings)
        :return: list of diphones (list of strings)
        """

        diphoneseq = []

        for i in range(len(phones) - 1):
            diphoneseq.append(phones[i] + "-" + phones[i + 1])

        return diphoneseq

    def file_creation(self, audioarray, dest, volume, reverse=False, play = False, temp = False):

        """Creates a .wav file for an utterance; doesn't return anything
        :param audioarray: audio data from 'synthesise' method (list)
        :param dest: filename (string)
        :param volume: volume (int between 0-100)
        :param reverse: should audioarray be reversed or not
        :param play: args argument if the file should play or not
        """

        output = wave.open(dest, 'wb')
        output.setparams(self.params)

        if reverse == 'signal':
            for i in range(len(audioarray)-1, -1, -1):
                output.writeframes(audioarray[i][0])

        else:
            for i in range(len(audioarray)):
                output.writeframes(audioarray[i][0])

        output.close()

        if volume or play:
            out = simpleaudio.Audio()
            out.load(dest)

            if volume:
                out.rescale(volume * 0.01)  # input value is 0-100

            if play:
                out.play()

            out.save(dest)

        if temp:
            os.remove(dest)

class Utterance:

    mydict = cmudict.dict()  # dictionary for our pronunciations, making it visible for methods 'get_phone_seq' and 'find_stems'

    def __init__(self, phrase):
        """
        Constructor takes a phrase to process.
        :param phrase: a string which contains the phrase to process.
        """

        self.phrase = phrase

    def get_phone_seq(self, reverse=None):

        """
        Returns the phone sequence corresponding to the text in this Utterance (i.e. self.phrase)
        :param reverse:  Whether to reverse something.  Either "words", "phones" or None
        :return: list of phones (as strings)
        """

        if len(self.phrase.split()) != 1:
            for date in re.findall(r'\d+\/+\d+\/+\d+|\d+\/+\d+', self.phrase):
                newdate = self.mutate_date(self.phrase)
                self.phrase = self.phrase[:self.phrase.index(date)] + newdate + self.phrase[self.phrase.index(date) + len(date):]

        phoneseq = []

        ourphrase = re.sub(r'[\d]', '', self.phrase).translate(str.maketrans('', '', string.punctuation)).lower().split() # Note: not creating new object creates problems for testing

        if reverse == 'words':
            ourphrase.reverse()

        for word in ourphrase:
            try:
                for phone in Utterance.mydict[word][0]:
                    phoneseq.append(re.sub(r'[0-9]', '', phone.lower()))
            except KeyError:
                solution = self.find_stems(word) # if we can't find a word, check if it's an unrecognised compound
                if solution:
                    phoneseq.extend(solution)
                else:
                    return f'Cannot find pronunciation for {word}; ending program.'

        if reverse == 'phones':
            phoneseq.reverse()

        return phoneseq

    def find_stems(self, word):

        """
        In cases where a word cannot be found in the cmudict, it gets thrown here, and this method works on the assumption
        that the word is a compound and tries to find its two stems
        :param word: a string inherited from get_phone_seq method
        :return: list of phones (as strings) OR boolean False if no match is found, which terminates the program
        """

        parsed_seq = []

        n = round((len(word)) / 2)
        k = 1
        for i in range(len(word) - 2):
            k = k * (-1)
            n = n + (i * k)
            try:
                hold = [] #temp repository, unless both words are found this won't get pushed

                for phone in Utterance.mydict[word[:n]][0]:
                    hold.append(re.sub(r'[0-9]', '', phone.lower()))
                for phone in Utterance.mydict[word[n:]][0]:
                    hold.append(re.sub(r'[0-9]', '', phone.lower()))

                parsed_seq.extend(hold)
                break

            except KeyError:
                continue

        return parsed_seq

    def mutate_date(self, dates):

        """
        This method is in charge of converting DD/MM/YY(YY) formatted dates into proper words
        :param dates: Gets a date to parse from Utterance.get_phone_seq
        """

        int2date = {
            'Day': {1: 'first', 2: 'second', 3: 'third', 4: 'fourth', 5: 'fifth', 6: 'sixth', 7: 'seventh',
                    8: 'eighth', 9: 'ninth', 10: 'tenth', 11: 'eleventh', 12: 'twelfth', 13: 'thirteenth',
                    14: 'fourteenth', 15: 'fifteenth', 16: 'sixteenth', 17: 'seventeenth', 18: 'eighteenth',
                    19: 'nineteenth', 20: 'twentieth', 21: 'twenty-first', 22: 'twenty-second', 23: 'twenty-third',
                    24: 'twenty-fourth', 25: 'twenty-fifth', 26: 'twenty-sixth', 27: 'twenty-seventh',
                    28: 'twenty-eighth', 29: 'twenty-ninth', 30: 'thirtieth', 31: 'thirty-first'},

            'Month': {1: 'january', 2: 'february', 3: 'march', 4: 'april', 5: 'may', 6: 'june', 7: 'july', 8: 'august',
                      9: 'september', 10: 'october', 11: 'november', 12: 'december'},

            'Year': {0: 'hundred', 1: 'one', 2: 'two', 3: 'three', 4: 'four', 5: 'five', 6: 'six', 7: 'seven',
                     8: 'eight', 9: 'nine', 10: 'ten', 11: 'eleven', 12: 'twelve', 13: 'thirteen', 14: 'fourteen',
                     15: 'fifteen', 16: 'sixteen', 17: 'seventeen', 18: 'eighteen', 19: 'nineteen', 20: 'twenty',
                     30: 'thirty', 40: 'forty', 50: 'fifty', 60: 'sixty', 70: 'seventy', 80: 'eighty', 90: 'ninety'}}

        dates = [int(x) for x in re.findall(r'[0-9]+', dates)]  # turning all strings to ints for our dictionary search
                                                                # takes care of numbers with leading zeros, e.g. '01' -> 1


        if len(dates) == 2: #dates DD/MM
            dates = int2date['Month'][dates[1]] + " " + int2date['Day'][dates[0]]

        elif len(dates) == 3: #dates DD/MM/YY(YY)
            if dates[-1] >= 1900:
                dates[-1] -= 1900
            dates.append(int((dates[-1] / 10)) * 10)
            dates[-2] = dates[-2] - dates[-1] #list positions: [0] = Day, [1] = Month, [2] = Year(SINGULAR), and [3] = Year(DECADE)

            try:
                if dates[3] > 10 and dates[2] != 0: #most dates fall within this category
                    dates = int2date['Month'][dates[1]] + " " + int2date['Day'][dates[0]] + " nineteen " + int2date['Year'][dates[3]] + " " + int2date['Year'][dates[2]]
                elif (dates[3] >= 10 or dates[2] == 0) and dates[2] == 0: #full decades
                    dates = int2date['Month'][dates[1]] + " " + int2date['Day'][dates[0]] + " nineteen " + int2date['Year'][dates[3]]
                elif dates[3] == 10 and dates[2] != 0:
                    dates = int2date['Month'][dates[1]] + " " + int2date['Day'][dates[0]] + " nineteen " + int2date['Year'][dates[2] + dates[3]]
                else: #years 1901-1909, adds a bit of style
                    dates= int2date["Month"][dates[1]] + " " + int2date["Day"][dates[0]] + " nineteen o " + int2date["Year"][dates[2]]

            except KeyError:
                print("Date out of range (supposed to be between 01/01/1900 - 31/12/1999. Terminating.")
                exit()

        return str(dates)


def process_file(textfile, args):

    """
    Takes the path to a text file and synthesises each sentence it contains
    :param textfile: the path to a text file (string)
    :param args: the parsed command line argument object giving options
    """

    full_file = []

    diphone_synth = Synth(wav_folder=args.diphones)

    try:
        with open(textfile, 'r') as file:
            for line in file:
                utt = Utterance(phrase=line)
                phone_seq = utt.get_phone_seq(reverse=args.reverse)
                full_file.extend(phone_seq) #removing silences for the full file synthesis
                diphone_synth.synthesise(phones=phone_seq, reverse=args.reverse, smooth_concat=args.crossfade)

    except FileNotFoundError:
        print(f"File '{textfile}' not found. Terminating.")
        exit()

    return full_file

def main(args):

    """Divided into four steps:
    (1) Load diphone wav. files from a specified folder
    (2) Parse our utterance
    (3) Feed these two into diphone_synth.synthesise
    (4) Finalise the process by playing and/or saving the file
    """

    diphone_synth = Synth(wav_folder=args.diphones) # (1)

    if not args.fromfile: # (2)
        utt = Utterance(phrase=args.phrase)
        phone_seq = utt.get_phone_seq(reverse=args.reverse)

    elif args.fromfile.endswith('.txt'):
        phone_seq = process_file(args.fromfile, args)

    else:
        print(f"It is not in our power to parse '{args.fromfile}'. Please provide a .txt file, else we be sad.")
        exit()

    audiodata = diphone_synth.synthesise(phones=phone_seq, reverse=args.reverse, smooth_concat=args.crossfade) # (3)

    if args.outfile: # (4)
        if not args.outfile.endswith('.wav'):
            args.outfile = datetime.utcnow().strftime("%Y-%m-%d %H:%M:%S") + ' UTC.wav' #providing a temp name if an improper outfile name has been given
        diphone_synth.file_creation(audioarray=audiodata, dest=args.outfile, volume=args.volume, reverse=args.reverse, play=args.play, temp=False)
        print(f"Your synthesised file '{args.outfile}' was created!")

    else:
        args.outfile = datetime.utcnow().strftime("%Y-%m-%d %H:%M:%S") + ' UTC.wav'  # bit of lazy hackery; a file is created, played, and immediately destroyed ('temp' parameter)
        diphone_synth.file_creation(audioarray=audiodata, dest=args.outfile, volume=args.volume, reverse=args.reverse, play=args.play, temp=True)

    print(f"Done!")

# DO NOT change or add anything below here
# (it just parses the commandline and calls your "main" function
# with the options given by the user)
if __name__ == "__main__":
    main(process_commandline())