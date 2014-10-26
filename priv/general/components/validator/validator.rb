#!/usr/bin/env ruby

require 'json'

acc = ""
STDIN.each_line("}") do |new_input|
  acc += new_input
  begin
    msg_obj = JSON.parse acc
  rescue
  else
    acc = ""
    if msg_obj["input"].nil? || msg_obj["output"].nil? then
      STDOUT.puts "ERROR"
      break
    else
      response = { "score" => rand(100)-1, "caption" => msg_obj["input"].join(" ") }
      STDOUT.puts JSON(response)
    end
    STDOUT.flush
  end
end
