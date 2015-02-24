
#! /usr/bin/env ruby

require 'csv'
require 'ostruct'
require 'optparse'

options = OpenStruct.new
options.output = Dir.pwd

parser = OptionParser.new do |opts|
  opts.banner = "Usage: sync.rb [options] <caliper file> <sychronized samples>"

  opts.on("-o", "--output [DIRECTORY]", "Output directory (default: current directory") do |file|
    options.output = IO.new(IO.sysopen(file, "w"), "w")
  end

  opts.on("-h", "--help", "Show this message") do
    puts opts
    exit
  end
end

parser.parse!

#if 2 != ARGV.count
#  puts parser
#  exit  
#end

csv_options = { 
  :col_sep => ",", 
  :headers => true,
  :header_converters => :symbol, 
  :converters => :numeric 
}


subjects = CSV.enum_for(:foreach, ARGV[0], csv_options).lazy.map do |row|
  OpenStruct.new(:run => row[:run], 
		 :name => row[:subject], 
                 :range => row[:stop]-row[:start],
                 :csv => CSV.open(row[:subject]+row[:run].to_s, mode = "w",
                                  :write_headers => true, 
                                  :headers => ["cpu", "hdd", "ram", "bridge", "tsc"]))
end

exetimes = Hash.new(0)
reptimes = Hash.new(0)

begin

  while true
    
    subject = subjects.next
    # save acummulative execution time
    name = subject.name
   
    #puts name
    acum_exetime = exetimes[name]
    rep_time = reptimes[name]
    
    #puts acum_exetime
    acum_exetime = acum_exetime + subject.range
    rep_time = rep_time + 1

    #puts acum_exetime
    exetimes[name] = acum_exetime
    reptimes[name] = rep_time

  end
  
rescue StopIteration => ex
  #puts ex
end

exetimes.each {|key, value| puts "#{key}, #{value/reptimes[key]}"}

#exetimes.each {|key, value| puts "#{key} is #{value}"}



