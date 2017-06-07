#!/usr/bin/env ruby

require "netrc"

if ARGV.size != 1
  $stderr.puts "Usage: #{$0} machine-name"
  exit 1
end

machine = ARGV[0]

puts Netrc.read[machine].password
