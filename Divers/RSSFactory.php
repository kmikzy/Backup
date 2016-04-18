<?php
/**
 * Manipulation des flux RSS rapidement via PHP
 * @author Xavier Van de Woestyne & Maxime Legrand
 * @version 2.0
 */
namespace RSSFactory;
class Channel{

  private $titre;
  private $lien;
  private $description;
  private $langue;
  private $image;
  
  public function __construct($titre, $lien, $description, $langue, $image){
    $this->titre = $titre;
    $this->lien = $lien;
    $this->description = $description;
    $this->langue = $langue;
    $this->image = $image;
  }
  public function getTitre(){return $this->titre;}
  public function getLien(){return $this->lien;}
  public function getDescription(){return $this->description;}
  public function getLangue(){return $this->langue;}  
  public function getImage(){return $this->image;}
}
class Item{

  private $titre;
  private $lien;
  private $description;
  private $auteur;
  private $categorie;
  private $commentaire;
  private $guid;
  private $date;
  
  public function __construct(){
    $this->titre = null;
    $this->lien = null;
    $this->description = null;
    $this->auteur = null;
    $this->categorie = null;
    $this->commentaire = null;
    $this->guid = null;
    $this->date = null;
  }

  public function getTitre(){ return $this->titre; }
  public function getLien(){ return $this->lien; }
  public function getDescription(){ return $this->description; }
  public function getAuteur(){ return $this->auteur; }
  public function getCategorie(){ return $this->categorie; }
  public function getCommentaire(){ return $this->commentaire; }
  public function getGuid(){ return $this->guid; }
  public function getDate(){ return $this->date; }
  
  public function setTitre($var){$this->titre = $var;}
  public function setLien($var){$this->lien = $var;}
  public function setDescription($var){$this->description = $var;}
  public function setAuteur($var){$this->auteur = $var;}
  public function setCategorie($var){$this->categorie = $var;}
  public function setCommentaire($var){$this->commentaire = $var;}
  public function setGuid($var){$this->guid = $var;}
  public function setDate($var){$this->date = $var;
  }
}
  function clearItem(){
    return new Item();
  }
  function fullItem($titre, $lien, $description, $auteur, $date, $categorie, $commentaire, $guid){
    $item = new Item();
    $item->setAuteur($auteur);
    $item->setTitre($titre);
    $item->setLien($lien);
    $item->setDescription($description);
    $item->setDate($date);
    $item->setCategorie($categorie);
    $item->setCommentaire($commentaire);
    $item->setGuid($guid);
    return $item;
  }
  function strictItem($titre, $lien, $description, $auteur, $date){
    $item = new Item();
    $item->setAuteur($auteur);
    $item->setTitre($titre);
    $item->setLien($lien);
    $item->setDescription($description);
    $item->setDate($date);
    return $item;
  }
class FluxRSS{

  private $channel;
  private $channelEx;
  private $listItem;
  private $rss;
  
  public function __construct(Channel $channelEx, Array $items){
    $this->channelEx = $channelEx;
    $this->listItem = $items;
    $this->buildRSSFlux();
  }
  public function getChannel(){return $this->channelEx;}
  public function fetch($nb = -1){
    $iteration = 0;
    foreach($this->listItem as $item){
      if($nb == 0)break;
      $newArray[] = $item;
      if($nb != -1)if((++$iteration>=$nb))break;
    }
    return $newArray;
  }  
  
  private function buildRSSFlux(){
    $this->rss = new \SimpleXMLElement('<rss/>');
    $this->rss->addAttribute('version', '2.0');
    $this->channel = $this->rss->addChild('channel');
    $this->channel->addChild('title', htmlentities($this->channelEx->getTitre()));
    $this->channel->addChild('link', htmlentities($this->channelEx->getLien()));
    $this->channel->addChild('description', htmlentities($this->channelEx->getDescription()));
    $this->channel->addChild('language', htmlentities($this->channelEx->getLangue()));
    $this->channel->addChild('image', htmlentities($this->channelEx->getImage()));
    foreach($this->listItem as $item){
      $newItem = $this->channel->addChild('item');
      $newItem->addChild('title', htmlentities($item->getTitre()));
      $newItem->addChild('link', htmlentities($item->getLien()));
      $newItem->addChild('description', htmlentities($item->getDescription()));
      if($item->getAuteur() !== null)$newItem->addChild('auteur', htmlentities($item->getAuteur()));
      if($item->getCategorie() !== null)$newItem->addChild('category', htmlentities($item->getCategorie()));
      if($item->getCommentaire() !== null)$newItem->addChild('comments', htmlentities($item->getCommentaire()));
      if($item->getGuid() !== null)$newItem->addChild('guid', htmlentities($item->getGuid()));
      if($item->getDate() !== null)$newItem->addChild('pubDate', htmlentities($item->getDate()));
    }
  }
  
  public function display(){
    header('Content-Type: application/xml');
    echo $this->rss->asXML();
  }
  
  public function save($chemin){
    $this->rss->asXML($chemin);
  }
}
function fetchExternRSS($fluxRSS){
  
  $curlConnexion = curl_init();
  curl_setopt($curlConnexion, CURLOPT_URL, $fluxRSS);
  curl_setopt($curlConnexion, CURLOPT_POST, 1);
  curl_setopt($curlConnexion, CURLOPT_RETURNTRANSFER, 1);
  $xmlExternString = curl_exec($curlConnexion);
  
  $rss = simplexml_load_string($xmlExternString);  
  $channel_titre = htmlentities($rss->channel->title);
  $channel_lien = htmlentities($rss->channel->link);
  $channel_description = htmlentities($rss->channel->description);
  $channel_image = htmlentities($rss->channel->image);
  $channel_langue = htmlentities($rss->channel->language);
  $channel = new Channel($channel_titre, $channel_lien, $channel_description, $channel_langue, $channel_image);
  foreach($rss->channel->item as $item){
    $listItem[] = strictItem(
      (string)(htmlentities($item->title)), 
      (string)(htmlentities($item->link)), 
      (string)(htmlentities($item->description)), 
      (string)(htmlentities($item->auteur)), 
      (string)(htmlentities($item->date)));
  }
  return new FluxRSS($channel, $listItem);
}
?>
