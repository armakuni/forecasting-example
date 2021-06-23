require 'stringio'

describe Forecasting::MonteCarlo do
  describe '#run' do
    let(:iterations) { 1 }
    let(:workers) { 1 }
    let(:performance) do
      [
        { complexity: 1, lower: 1.0, upper: 2.0, distribution: 'uniform' }
      ]
    end
    let(:backlog) do
      [
        { name: 'task 1', complexity: 1 }
      ]
    end
    let(:perf) { Forecasting.to_object_hash(performance, :complexity) }
    let(:blog) { Forecasting.to_object_array(backlog) }

    subject do
      described_class.new(perf)
    end

    context 'with a simple backlog' do
      before do
        subject.run(backlog: blog, iterations: iterations, workers: workers)
      end
      it 'reports successfully' do
        io = StringIO.new
        subject.report(io)
        expect(io.string).to match /After 1 iterations with 1 worker/
      end
    end
  end
end
