#!/usr/bin/env ruby

require 'json'

def change_state state, proposition
  # dummy implementation: string-concat
  state + proposition
end

acc = ""
STDIN.each_line do |new_input|
  acc += new_input
  begin
    msg_obj = JSON.parse acc
  rescue JSON::ParserError
  else
    if msg_obj["proposition"].nil? || msg_obj["state"].nil? then
      STDOUT.puts "ERROR"
      break
    else
      response = { "state" => change_state(msg_obj["state"], msg_obj["proposition"]) }
      STDOUT.puts JSON(response)
    end
    STDOUT.flush
  ensure
    acc = ""
  end
end
