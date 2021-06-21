# frozen_string_literal: true

require 'pathname'
require 'yaml'
require 'ostruct'

$: << Pathname.new(__FILE__).parent.parent + 'lib'
require 'forecasting'

backlog = Forecasting.load_backlog('backlog.yml')
performance = Forecasting.load_performance('performance.yml')

iterations = 100000
workers = backlog.map{ |b| b[:worker] }.uniq # derive workers from backlog assignments
critical_path = [0] * workers.size
samples = []

# For fully assigned work, each worker moves through the priority-ordered backlog items assigned to them
# The critical path becomes the worker with the longest total elapsed time.
iterations.times do
  worker_elapsed = [0.0] * workers.size
  workers.each_index do |idx|
    backlog.each do |task|
      next unless task['worker'] == workers[idx]
      complexity = performance[task.complexity]
      elapsed = Distribution::Triangular.random(complexity.mode, complexity.lower, complexity.upper)
      worker_elapsed[idx] += elapsed
    end
  end
  # choose worker with the longest elapsed time, take that time as critical path
  idx = worker_elapsed.index(worker_elapsed.max)
  samples << worker_elapsed[idx] 
  critical_path[idx] += 1 # and let's record how many times this worker was critical path
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
puts
puts "Critical path workers:"
critical_path.each_index do |idx|
  puts "  Worker #{workers[idx]}: %05.2f%% (#{critical_path[idx]})" % (critical_path[idx] * 100.0 / iterations)
end