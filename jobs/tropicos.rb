require 'httparty'

SCHEDULER.every '60s', :first_in => 0 do |job|
  result = HTTParty.get('http://services.tropicos.org/Name/25509881?apikey=f3e499d4-1519-42c9-afd1-685a16882f5a&format=json')
  
	if result['NameId'] == 25509881
			out = "BOOM! It's up"
			# out = 100
		else
			out = "Sad. It's down"
			# out = 0
		end
  
  send_event('tropicos', { text: out })
end