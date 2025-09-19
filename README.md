# Voluntary-Language-Switch
Project for lab rotation cognition and knowledge at RPTU

## 1. Data preprocessing

### 1.1 Reaction time

#### Chronset RT
[Chronset](https://www.bcbl.eu/databases/chronset) was used to obtain the RT of each audio file.
The RT results were obtained and stored as *.txt* for each participant, e.g., *chronset_1.txt*

#### Mannual RT
RT is accessed mannually using Audacity for a few trails in mixed block for all the participants. The mannual results were compared with the Chronset results to ensure the quality of Chronset RT.

### 1.2 Content of response

The content of the audio files are first analyzed using Python (*speech_rec.py* and *speech_rec_whisper.py*).

* <span style="color:tomato">**speech_rec.py**</span>: using *speech_recognition* in Python to produce output files *english_text.csv* and can be adapted to get *german_text.csv* by changing the output file name and language:

    `text = recognizer.recognize_google(audio, language="de-DE")`

    The expected running time is around 3 hours for each language.

* <span style="color:tomato">**speech_rec_whisper.py**</span>: using *whisper* in Python to produce output file *transcriptions.csv*, and the expected running time is about 50 hours. 

Note: these two methods worked in a complementory way with the first one being more efficient and accurate. Of course, not all the audio files are accurately recognized. So, a mannual appraoch was adopted afterwards for the inaccurate ones.

### 1.3 Organizing data

<span style="color:tomato">**vls_preproc.R**</span> was used to combine subject files, chronset files, and speech output, as well as rescale rating and obtain type of response etc.

- Combine all subject files
- Merge the RT from chronset files to main dataframe
- Rename and re-scale the participant rating responses
- Merge speech recognization outputs to main df
- Combine manually retrieved data
- Obtain response _accuracy_, _language_, and _type_.
- Add standard valence scale and compare the response valence rating to the standard scale
    - One-sample t test was used to compare the valence between paticipant response and standrd scale.
    - No significant differences were identified.
- Compare chronsst RT with manual RT in Mixed blocks
    - Normality was always violated, so Wilcoxon signed-rank test was used to compared Chronset RT and manual RT.
    - Significant differences were found between Chronst and manual RTs.

## 2. Data analysis
Details of the statistical analysis can be found in the file <span style="color:tomato">**vls_analysis.R**</span>.
