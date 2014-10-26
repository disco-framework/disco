#!/usr/bin/env ruby

require 'json'

def fiblist n
  n <= 2 ? n :
    (1..n-2).inject([1,1]) {|acc,n| acc + [acc[-1]+acc[-2]]}
end

def workerinput problemspec
  # dummy implementation.
  # you can see though that a list of input strings should be returned
  # these are joined (with "\n" as a divider) for transport to the worker
  liststring, numberstring = problemspec.split(" ")
  [eval(liststring).to_a.to_s.delete(" "), numberstring]
end

acc = ""
STDIN.each_line do |new_input|
  acc += new_input
  begin
    msg_obj = JSON.parse acc
  rescue JSON::ParserError
  else
    # attribute "state" is optional, "problem" is mandatory
    if msg_obj["problem"].nil? then
      STDOUT.puts "ERROR"
      break
    else
      # just an example, should work for "the countdown problem" though
      response = { "worker input" => workerinput(msg_obj["problem"]) }
      STDOUT.puts JSON(response)
    end
    STDOUT.flush
  ensure
    acc = ""
  end
end
