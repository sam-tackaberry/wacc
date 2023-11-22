#!/usr/bin/env ruby

expected = "expected.txt"
actual = "actual.txt"
input = "input.txt"

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
  ["src/test/testcases/valid/", 0],
  ["src/test/testcases/invalid/syntaxErr/", 100],
  ["src/test/testcases/invalid/semanticErr/", 200]
]

for testcase in testcases do

  puts "+---------------------------------------------------------------------------------------+"
  puts "\t" + "Running tests in directory: #{testcase[0]}".pink
  puts "+---------------------------------------------------------------------------------------+"
  puts ""
  files = Dir.glob("#{testcase[0]}**/*.wacc")

  for file in files do
      filename = File.basename(file)
      fileNoExt = File.basename(file, ".wacc")
      puts "\tRunning: #{filename}".light_blue
      %x(./compile #{file})
      exit_code = $?.exitstatus
      total_tests += 1

      if testcase[1] != exit_code then
          puts "\tTest Failed: #{exit_code}".red
          puts "\tExpected: #{testcase[1]}".red
          tests_failed += 1
      else
        #Don't run code generation on invalid or advanced tests
        if exit_code == 0 && !file.include?("advanced") then

          #Remove starting hashtags from before expected input and output
          File.write(expected, File.read(file).gsub(/^#\s/, ''))

          #Write all input into input.txt
          File.open(expected) do |f|
            input_lines = f.select { |line| line.start_with?("Input:") }.map { |line| line.sub(/^Input:\s*/, '') }
            File.write(input, input_lines.join) unless input_lines.empty?
          end

          #Write all expected output into expected.txt
          inside_output = false
          File.write(expected, File.read(expected).lines.select { |line|
            if line.start_with?("Output:")
              inside_output = true
            elsif inside_output && line.strip.empty?
              inside_output = false
            end
            inside_output && !line.start_with?("Output:")
          }.join)
        
          #Remove empty lines from expected output
          File.write(expected, File.read(expected).lines.reject(&:empty?).join) 
          
          system("arm-linux-gnueabi-gcc -o #{fileNoExt} -mcpu=arm1176jzf-s -mtune=arm1176jzf-s #{fileNoExt}.s")
          system("(qemu-arm -L /usr/arm-linux-gnueabi/ #{fileNoExt} < #{input}) > #{actual}")
          
          #Remove all addresses and runtime error messages from actual output
          File.write(actual, File.read(actual).gsub(/^fatal error:.*/, "#runtime_error#"))
          File.write(actual, File.read(actual).gsub(/0x[\da-f]*/, '#addrs#'))

          file1 = File.open(actual, "r")
          file2 = File.open(expected, "r")
          first_difference = true

          # Compare the files
          file1.each_line.with_index do |line, index|
            if line.chomp != file2.readline.chomp
              if first_difference
                puts "\tTest Failed: #{exit_code}".red
                tests_failed += 1
                first_difference = false
              end
              puts "\tFiles differ at line #{index + 1}:"
              puts "\t< #{line.chomp}"
              puts "\t> #{file2.readline.chomp}"
            end
          end

          if first_difference
            puts "\tTest Passed!".green
          end

          # Close the files
          file1.close
          file2.close

          # Delete assembly and binary files
          # File.delete(fileNoExt)
          # File.delete(expected)
          # File.delete(actual)
          # File.delete(fileNoExt + ".s")
        else
          puts "\tTest Passed!".green
        end
        
      end
      puts ""
  end
end

tests_passing = (total_tests - tests_failed)
# File.delete(input)

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

exit 0
end
