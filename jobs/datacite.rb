require 'httparty'

SCHEDULER.every '60s', :first_in => 0 do |job|
  result = HTTParty.get('http://oai.datacite.org/oai?verb=Identify')

	if result['OAI_PMH']['request']['__content__'] == 'http://oai.datacite.org/oai'
			out = "BOOM! It's up"
			# out = 100
		else
			out = "Sad. It's down"
			# out = 0
		end
  
  send_event('datacite', { text: out })
end