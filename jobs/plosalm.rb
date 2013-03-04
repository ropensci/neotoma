require 'httparty'

# buzzwords = ['Paradigm shift', 'Leverage', 'Pivoting', 'Turn-key', 'Streamlininess', 'Exit strategy', 'Synergy', 'Enterprise', 'Web 2.0'] 
# buzzword_counts = Hash.new({ value: 0 })

SCHEDULER.every '60s', :first_in => 0 do |job|
  # random_buzzword = buzzwords.sample
  # buzzword_counts[random_buzzword] = { label: random_buzzword, value: (buzzword_counts[random_buzzword][:value] + 1) % 30 }
  result = HTTParty.get("http://alm.plos.org/articles/10.1371/journal.pbio.0000012.json?api_key=WQcDSXml2VSWx3P")['article']['doi']

	if result == "10.1371/journal.pbio.0000012"
			out = "BOOM! It's up"
			# out = 100
		else
			out = "Sad. It's down"
			# out = 0
		end
  
  send_event('plosalm', { text: out })
end