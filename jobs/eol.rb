require 'httparty'

SCHEDULER.every '60s', :first_in => 0 do |job|
  result = HTTParty.get('http://eol.org/api/ping/1.0.json')
  
	if result['response']['message'] == "Success"
			# out = "BOOM! It's up"
			out = 100
		else
			# out = "Sad face. It's down"
			out = 0
		end
  
  send_event('eol', { value: out })
end