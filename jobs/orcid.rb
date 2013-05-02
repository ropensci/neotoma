require 'httparty'
require 'crack'

SCHEDULER.every '60s', :first_in => 0 do |job|
	
	class Foo
	  include HTTParty
	  headers 'Accept' => 'application/orcid+xml'
	end
  
	  result = HTTParty.get('http://pub.orcid.org/search/orcid-bio/?q=family-name:Sanchez&start=1&rows=1')
	  res2 = Crack::XML.parse(result)
	  
		if res2['orcid_message']['xmlns'] == "http://www.orcid.org/ns/orcid"
				out = "BOOM! It's up"
				# out = 100
			else
				out = "Sad. It's down"
				# out = 0
			end
	  
	  send_event('orcid', { text: out })
end