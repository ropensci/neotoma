require 'httparty'

SCHEDULER.every '60s', :first_in => 0 do |job|
  result = HTTParty.get("http://api.altmetric.com/v1/citations/1d?num_results=1")

	if result['query']['page'] == 1
			out = "BOOM! It's up"
			# out = 100
		else
			out = "Sad. It's down"
			# out = 0
		end
  
  send_event('altmetric', { text: out })
end