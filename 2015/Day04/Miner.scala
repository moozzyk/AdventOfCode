import java.security.MessageDigest;

object Miner {
    def mine(key: String, criterion: String => Boolean) : Int = {
        return Stream.from(1).takeWhile(!checkMagicNumber(key, _, criterion)).length + 1;
    }

    def checkMagicNumber(key: String, magicNumber : Int, criterion: String => Boolean) : Boolean = {
        return criterion(calculateHash(key + magicNumber.toString));
    }

    def calculateHash(key: String) : String = {
        return MessageDigest.getInstance("MD5").digest(key.getBytes).map("%02X" format _).mkString;
    }
}