require 'httparty'

SCHEDULER.every '60s', :first_in => 0 do |job|
  result = HTTParty.get('http://data.gbif.org/ws/rest/taxon/get/100')

	if result['gbifResponse']['header']['request'] == 'get'
			out = "BOOM! It's up"
			# out = 100
		else
			out = "Sad. It's down"
			# out = 0
		end
  
  send_event('gbif', { text: out })
end