import whisper
import pandas as pd
import os
from tqdm import tqdm  # Progress bar
import torch
import warnings
warnings.filterwarnings("ignore", message="FP16 is not supported on CPU")

device = "cuda" if torch.cuda.is_available() else "cpu"
# Load Whisper model (Choose: tiny, base, small, medium, large)
model = whisper.load_model("medium", device=device)

# Directory containing WAV files
audio_dir = "/Users/ruohan/Documents/RPTU/WS 24:25/LB_Cognition/VLS/data/archive"  # Change this to your folder path

# Get all .wav files
wav_files = [f for f in os.listdir(audio_dir) if f.endswith(".wav")]

data = []

for file in tqdm(wav_files, desc="Processing audio files"):
    file_path = os.path.join(audio_dir, file)

    result = model.transcribe(file_path)
    print("the", file, "is being processed. Let's be patient")
    data.append([file, result["language"], result["text"]])


df = pd.DataFrame(data, columns=["File Name", "Language", "output_text"])


csv_path = "transcriptions.csv"
df.to_csv(csv_path, index=False)

print(f"Transcriptions saved to {csv_path}")
