module Forecasting
  class MonteCarlo
    def initialize(performance)
      @performance = performance
    end

    def run(backlog:, iterations: 1000, workers: 1)
      @workers = workers
      @iterations = iterations
      @samples = []

      iterations.times do
        worker_elapsed = [0.0] * workers
        backlog.each do |task|
          complexity = @performance[task.complexity]
          distribution = Distribution.select(complexity.distribution)
          elapsed = distribution.random(complexity.mode, complexity.lower, complexity.upper)
          worker_elapsed.sort!  # determine next worker to finish a task
          worker_elapsed[0] += elapsed
        end
        @samples << worker_elapsed.max # choose worker with longest  elapsed time
      end

      @samples.sort!
    end

    def percentile(pct)
      @samples[(@samples.length * pct / 100).to_i]
    end

    def report(io: STDOUT)
      io.puts "After #{@iterations} iterations with #{@workers} workers"
      io.puts '  50%% = %0.2f days' % percentile(50)
      io.puts '  80%% = %0.2f days' % percentile(80)
      io.puts '  90%% = %0.2f days' % percentile(90)
      io.puts '  95%% = %0.2f days' % percentile(95)
      io.puts
      io.puts format('90%% confidence range = %0.2f days', (percentile(95) - percentile(5)))
    end
  end
end
