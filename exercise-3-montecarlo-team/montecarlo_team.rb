# frozen_string_literal: true

require 'pathname'
require 'yaml'
require 'ostruct'

$: << Pathname.new(__FILE__).parent.parent + 'lib'
require 'forecasting'

backlog = Forecasting.load_backlog('backlog.yml')
performance = Forecasting.load_performance('performance.yml')

iterations = 100000
workers = 1
samples = []

iterations.times do
  worker_elapsed = [0.0] * workers
  backlog.each do |task|
    complexity = performance[task.complexity]
    elapsed = Distribution::Triangular.random(complexity.mode, complexity.lower, complexity.upper)
    worker_elapsed.sort!
    worker_elapsed[0] += elapsed # next worker to finish a task
  end
  samples << worker_elapsed.sort.last # choose worker with longest  elapsed time
end

samples.sort!

def percentile(samples, pct)
  samples[(samples.length * pct / 100).to_i]
end

puts "After #{iterations} iterations"
puts "  50%% = %0.2f days" % percentile(samples, 50)
puts "  80%% = %0.2f days" % percentile(samples, 80)
puts "  90%% = %0.2f days" % percentile(samples, 90)
puts "  95%% = %0.2f days" % percentile(samples, 95)
