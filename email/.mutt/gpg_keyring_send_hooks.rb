#!/usr/bin/env ruby

def raw_key_dump
  `gpg --list-keys --with-colons --fixed-list-mode --no-secmem-warning`
end

def email_matcher
  /.*\<(.*)\>/
end

def gpg_email_addresses
  raw_key_dump.scan(email_matcher).flatten.uniq
end

gpg_email_addresses.each do |address|
  puts "send-hook \"!~l ~t #{address}\" \"set pgp_autoencrypt\""
end
