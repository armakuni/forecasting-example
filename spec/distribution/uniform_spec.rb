describe Distribution::Uniform do
  describe '.random' do
    it 'returns a random number within bounds' do
      expect(described_class.random(10.0, 5.0, 15.0)).to be_between(5.0, 15.0)
    end

    it 'distributes evenly for large sample sizes' do
      sum = [0.0] * 10
      10_000.times do
        sum[described_class.random(0.0, 0.0, 10.0).to_i] += 1
      end
      expect(sum.max).to be_within(100).of(1000)
      expect(sum.min).to be_within(100).of(1000)
    end
  end
end
