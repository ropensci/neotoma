require 'httparty'

SCHEDULER.every '60s', :first_in => 0 do |job| 
  result = HTTParty.get("http://opensnp.org/snps/json/rs9939609/1.json")

	if result['snp']['name'] == "rs9939609"
			out = "BOOM! It's up"
			# out = 100
		else
			out = "Sad. It's down"
			# out = 0
		end
  
  send_event('opensnporg', { text: out })
end