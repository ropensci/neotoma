require 'httparty'

SCHEDULER.every '60s', :first_in => 0 do |job|
  result = HTTParty.get('http://www.fishbase.us/maintenance/FB/showXML.php?identifier=FB-2&ProviderDbase=03')

	if result['response']['taxon']['Kingdom'] == 'Animalia'
			out = "BOOM! It's up"
			# out = 100
		else
			out = "Sad. It's down"
			# out = 0
		end
  
  send_event('fishbase', { text: out })
end