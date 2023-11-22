#!/usr/bin/env ruby

class String
    def colorize(color_code)
      "\e[#{color_code}m#{self}\e[0m"
    end
  
    def red
      colorize(31)
    end
  
    def cool
      colorize(45)
    end
  
    def green
      colorize(32)
    end
  
    def light_blue
      colorize(36)
    end
  
    def pink
        colorize(35)
    end
  end
  
  total_tests = 0
  
  tests_failed = 0
  
  testcases = [
    # ["src/test/testcases/valid/", 0],
    ["src/test/testcases/invalid/syntaxErr/", 100],
    # ["src/test/testcases/invalid/semanticErr/", 200]
  ]
  
  for testcase in testcases do
  
    puts "+---------------------------------------------------------------------------------------+"
    puts "\t" + "Running tests in directory: #{testcase[0]}".pink
    puts "+---------------------------------------------------------------------------------------+"
    puts ""
    files = Dir.glob("#{testcase[0]}**/*.wacc")
  
    for file in files do
        filename = File.basename(file)
        puts "\tRunning: #{filename}".light_blue
        %x(./compile #{file})
        exit_code = $?.exitstatus
        total_tests += 1
  
        if testcase[1] != exit_code then
            puts "\tTest Failed: #{exit_code}".red
            puts "\tExpected: #{testcase[1]}".red
            tests_failed += 1
        else
            puts "\tTest Passed!".green
        end
        puts ""
    end
  end
  
  puts "+----------------------------------+"
  puts "\t" + "Test suite completed".cool
  puts "+----------------------------------+"
  puts ""
  
  puts "+----------------+"
  puts "| Result: #{(((total_tests - tests_failed) / total_tests.to_f) * 100).round(2)}% |"
  puts "| Result: #{total_tests - tests_failed} / #{total_tests}  |"
  puts "+----------------+"
  
  puts ""
  
  if tests_failed != 0 then
  puts "SOME TESTS FAILED STOPPING HERE".red
  exit tests_failed
  else
  
  puts "STARTING SCALA TEST SUITE".green
  puts ""
  result = system './bin/Test'
  
  if result then
    exit -1
  else
    exit 0
  end
  end
  