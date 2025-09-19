
import os
import speech_recognition as sr
import pandas as pd
from pydub import AudioSegment

# Function to convert to PCM WAV
def convert_to_wav(file_path, output_folder="converted_audio"):
     os.makedirs(output_folder, exist_ok=True)
     audio = AudioSegment.from_file(file_path)
     audio = audio.set_frame_rate(16000).set_channels(1) # Ensure 16kHz mono
     file_name = os.path.basename(file_path).replace(".wav", "_converted.wav")
     new_file = os.path.join(output_folder, file_name)
     audio.export(new_file, format="wav")
     return new_file

# Set directory with WAV files
audio_directory = "/Users/ruohan/Documents/RPTU/WS 24:25/LB_Cognition/VLS/data"
recognizer = sr.Recognizer()
data = []

for filename in os.listdir(audio_directory):
    if filename.endswith(".wav"):
        file_path = os.path.join(audio_directory, filename)
        print(f"Processing: {filename}")

        # Convert before recognition
        converted_file = convert_to_wav(file_path)

        with sr.AudioFile(converted_file) as source:
            audio = recognizer.record(source)
        
        try:
            text = recognizer.recognize_google(audio)
            print(f"Extracted Text: {text[:50]}...")
            data.append({"filename": filename, "english_text": text})
        except sr.UnknownValueError:
            data.append({"filename": filename, "english_text": "Unrecognized"})
        except sr.RequestError:
            data.append({"filename": filename, "english_text": "API Error"})

df = pd.DataFrame(data)
df.to_csv("english_text.csv", index=False)
print("Transcriptions saved to english_text.csv")
