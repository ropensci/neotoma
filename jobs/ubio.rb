require 'httparty'

SCHEDULER.every '60s', :first_in => 0 do |job|
  result = HTTParty.get('http://www.ubio.org/webservices/service.php?function=namebank_object&namebankID=2483153&keyCode=b052625da5f330e334471f8efe725c07bf4630a6')
  
	if result['results']['namebankID'] == "2483153"
			out = "BOOM! It's up"
			# out = 100
		else
			out = "Sad. It's down"
			# out = 0
		end
  
  send_event('ubio', { text: out })
end