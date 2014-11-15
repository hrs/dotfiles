import os, re

FOLDER_MAP = {
  "thoughtbot": {
    "drafts": "[Gmail]/Drafts",
    "sent": "[Gmail]/Sent Mail",
    "archive": "[Gmail]/All Mail"
  },
  "personal": {
    "drafts": "INBOX.Drafts",
    "sent": "INBOX.Sent Items",
    "archive": "INBOX.Archive"
  }
}

THOUGHTBOT_EXCLUDED_FOLDERS = ["[Gmail]/Trash", "[Gmail]/Important", "[Gmail]/Spam"]
PERSONAL_EXCLUDED_FOLDERS = ["INBOX.Trash", "INBOX.Junk Mail"]
EXCLUDED_FOLDERS =  THOUGHTBOT_EXCLUDED_FOLDERS + PERSONAL_EXCLUDED_FOLDERS

def inverse_map(map):
  return {v:k for k,v in map.items()}

def local_to_remote_mapper(account):
  def local_folder_to_remote_folder(folder):
    return FOLDER_MAP[account].get(folder, folder)
  return local_folder_to_remote_folder

def remote_to_local_mapper(account):
  def remote_folder_to_local_folder(folder):
    return inverse_map(FOLDER_MAP[account]).get(folder, folder)
  return remote_folder_to_local_folder

def should_include_folder(folder):
  return folder not in EXCLUDED_FOLDERS and not folder.startswith("RESTORED")
