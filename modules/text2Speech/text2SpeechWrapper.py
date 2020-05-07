
import requests as req
import json
import os
import base64

class Text2SpeechApiWrapper:

  def __init__(self):
    self.key_filename = 'apiKey'
    self.audio_save_folder_name = 'audio'
    self.was_setup = False
    self.dirname = os.path.dirname(os.path.abspath(__file__))
    self.keypath = os.path.join(
      self.dirname,
      self.key_filename
    )
    self.audiopath = os.path.join(
      self.dirname,
      self.audio_save_folder_name
    )
    self.url = 'https://texttospeech.googleapis.com/v1/text:synthesize'
    self.audio_ext = 'mp3'

  def setup(self):
    with open(self.keypath, 'r') as fd:
      self.key = fd.read().strip() # such a stupid name for trim
    self.was_setup = True

  def run(self, text):
    if not self.was_setup:
      self.setup()

    payload = {
      'input':{
        'text': text
      },
      'voice':{
        'languageCode': 'en-gb',
        'name': 'en-GB-Standard-A',
        'ssmlGender': 'FEMALE'
      },
      'audioConfig':{
        'audioEncoding': self.audio_ext
      }
    }
    response = req.post(
      self.url,
      headers={
        'Content-Type': 'application/json; charset=utf-8',
        'Authorization': 'Bearer'
      },
      params={
        'key': self.key
      },
      data=json.dumps(payload)
    )

    if response.status_code > 300:
      raise Exception('invalid text to speech api call', response.status_code, response.content)

    data = response.json()
    base64Data = data['audioContent']
    return base64Data

  def convert2AudioFile(self, data, filename):
    filepath = os.path.join(self.audiopath, ''.join([filename, '.', self.audio_ext]))
    decoded = base64.b64decode(data)
    with open(filepath, 'wb') as fd:
      fd.write(decoded)
    return filepath

  def runAudio(self, path):
    # TODO: starfile doesn't allow for the opened file to
    # be closed, find a better way
    os.startfile(path, 'open')

if __name__ == '__main__':
  t2s = Text2SpeechApiWrapper()
  t2s.setup()
  data = t2s.run('So what are your doings for today, sir?...')
  path = t2s.convert2AudioFile(data, 'trial')
  t2s.runAudio(path)