#!/usr/bin/env ruby

while true do
  squares = nil
  board = nil

  squares = ARGF.gets.strip[1..-2].split(',').map(&:to_i)
  board = Integer(ARGF.gets.strip)

  STDOUT.puts "[(0,0,#{squares.sort.last})]"
  STDOUT.flush
end
