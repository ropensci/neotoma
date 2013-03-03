require 'httparty'

SCHEDULER.every '60s', :first_in => 0 do |job|
  result = HTTParty.get("http://www.itis.gov/ITISWebService/services/ITISService/getITISTermsFromScientificName?srchKey=helianthus%20annuus")

	if result['getITISTermsFromScientificNameResponse']['return']['itisTerms'][0]['tsn'] == '36616'
			# out = "BOOM! It's up"
			out = 100
		else
			# out = "Sad face. It's down"
			out = 0
		end
  
  send_event('itis', { value: out })
end