# frozen_string_literal: true

require 'pathname'
require 'yaml'
require 'ostruct'

$: << Pathname.new(__FILE__).parent.parent + 'lib'
require 'forecasting'

# In this exercise, we compare two backlogs with ostensibly the same average elapsed time. One backlog
# is a series of small tasks, with a narrow range of completion times. The second backlog has just one
# longer task - with lower and upper bounds that equal the sum of the smaller tasks. What you will see
# is that the uncertainty for the single task is greater than the backlog of smaller tasks.
#
# In real life, uncertainty increases exponentially as complexity grows, so you would expect to see
# massive uncertainties for large-granularity epics.

['backlog1.yml', 'backlog2.yml'].each do |backlog_file|
  backlog = Forecasting.load_backlog(backlog_file)
  performance = Forecasting.load_performance('performance.yml')

  iterations = 1000
  samples = []

  iterations.times do
    total_elapsed = 0.0
    backlog.each do |task|
      complexity = performance[task.complexity]
      elapsed = Distribution::Triangular.random(complexity.mode, complexity.lower, complexity.upper)
      total_elapsed += elapsed
    end
    samples << total_elapsed
  end

  samples.sort!

  def percentile(samples, pct)
    samples[(samples.length * pct / 100).to_i]
  end

  puts "Backlog: #{backlog_file}: After #{iterations} iterations"
  puts "  50%% = %0.2f days" % percentile(samples, 50)
  puts "  80%% = %0.2f days" % percentile(samples, 80)
  puts "  90%% = %0.2f days" % percentile(samples, 90)
  puts "  95%% = %0.2f days" % percentile(samples, 95)
  puts
  puts " 5-95%% confidence = %0.2f days" % (percentile(samples, 95) - percentile(samples, 5))
  puts
end