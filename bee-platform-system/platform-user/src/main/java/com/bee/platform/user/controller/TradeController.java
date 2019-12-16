 package com.bee.platform.user.controller;

 import com.alibaba.fastjson.JSONObject;
 import com.alibaba.fastjson.TypeReference;
 import com.baomidou.mybatisplus.plugins.pagination.Pagination;
 import com.bee.platform.business.dto.CountDTO;
 import com.bee.platform.business.dto.ProductDTO;
 import com.bee.platform.business.dto.SaleProductDTO;
 import com.bee.platform.common.entity.Page;
 import com.bee.platform.common.entity.ResCodeEnum;
 import com.bee.platform.common.entity.ResponseResult;
 import com.bee.platform.common.service.ConfigService;
 import com.bee.platform.common.utils.PageUtils;
 import com.bee.platform.user.utils.RequestUtils;
 import io.swagger.annotations.Api;
 import io.swagger.annotations.ApiImplicitParam;
 import io.swagger.annotations.ApiImplicitParams;
 import io.swagger.annotations.ApiOperation;
 import lombok.extern.slf4j.Slf4j;
 import org.springframework.beans.factory.annotation.Autowired;
 import org.springframework.beans.factory.annotation.Value;
 import org.springframework.web.bind.annotation.CrossOrigin;
 import org.springframework.web.bind.annotation.GetMapping;
 import org.springframework.web.bind.annotation.RequestMapping;
 import org.springframework.web.bind.annotation.RestController;

 import java.util.List;


 /**
  * 资讯接口类
 * @author chenxm66777
 * @date 2019/03/04
 */
 @Api(value = "贸易平台API", tags = "贸易平台API")
 @RestController
 @CrossOrigin(origins = "*")
 @Slf4j
 @RequestMapping("/api/trade")
public class TradeController {

     @Value("${beetrade.addr}")
     private String addr;

     @Value("${beetrade.inquiryInfoListForPlatform}")
     private String inquiryInfoListForPlatform;

     @Value("${beetrade.saleInfoListForPlatform}")
     private String saleInfoListForPlatform;

     @Value("${beetrade.tradeCount}")
     private String tradeCount;

     @Autowired
     private ConfigService configService;

     @ApiImplicitParams({
             @ApiImplicitParam(name="type",value="0全部  1合金 2泵阀 3钢料 4机械设备 5其他",required=true)
     }
     )
     @GetMapping(value="/getInquiryInfoListForPlatform")
     @ApiOperation(value="首页-获取询价信息列表(平台调用)",notes="首页-获取询价信息列表(平台调用)")
     public ResponseResult<List<ProductDTO>> getInquiryInfoListForPlatform(Integer type, Page page){

         //获取首页获取询价信息调用贸易平台地址
         String url  = addr+inquiryInfoListForPlatform;
         Pagination pagination= PageUtils.transFromPage(page);
         //组装请求地址
         url = url +"?type="+ type +"&currentPage="+pagination.getCurrent() +"&pageSize="+pagination.getSize()+"";
         JSONObject returnJson =  RequestUtils.sendGetRequest(url);
         List<ProductDTO> returnProduct = null;
         try {
             returnProduct = JSONObject.parseObject(returnJson.getString("data"), new TypeReference<List<ProductDTO>>() {});
             return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,returnProduct, PageUtils.transToPage(pagination));
         }catch (Exception e){
             return ResponseResult.buildResponseResult(ResCodeEnum.TRADE_ERROR,returnProduct, PageUtils.transToPage(pagination));
         }
     }


     @GetMapping(value="/getSaleInfoListForPlatform")
     @ApiOperation(value="首页-获取现货信息列表(平台调用)",notes="获取现货信息列表(可分页)(平台调用)")
     @ApiImplicitParams({
             @ApiImplicitParam(name="type",value="0全部  1合金专区  2矿石专区 3辅料专区 4五金专区 5其他",required=true)
     }
     )
     public ResponseResult<List<SaleProductDTO>> getSaleInfoListForPlatform(Integer type,Page page){
         //获取首页现货询价信息调用贸易平台地址
         String url  = addr+tradeCount;
         Pagination pagination=PageUtils.transFromPage(page);
         //组装请求地址

         JSONObject returnJson =  RequestUtils.sendGetRequest(url);
         List<SaleProductDTO> returnProduct  = null;
         try {
             returnProduct = JSONObject.parseObject(returnJson.getString("data"), new TypeReference<List<SaleProductDTO>>() {});
             return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,returnProduct, PageUtils.transToPage(pagination));
         }catch (Exception e){
             return ResponseResult.buildResponseResult(ResCodeEnum.TRADE_ERROR,returnProduct, PageUtils.transToPage(pagination));
         }
     }


     @GetMapping(value="/getTradeCount")
     @ApiOperation(value="首页-获取贸易数据统计",notes="首页-获取贸易数据统计")
     public ResponseResult<List<CountDTO>> getTradeCount(){
         //获取首页现货询价信息调用贸易平台地址
         String url  = addr+tradeCount;

         JSONObject returnJson =  RequestUtils.sendGetRequest(url);
         List<CountDTO> countList  = null;
         try {
             countList = JSONObject.parseObject(returnJson.getString("data"), new TypeReference<List<CountDTO>>() {});
             return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,countList);
         }catch (Exception e){
             return ResponseResult.buildResponseResult(ResCodeEnum.TRADE_ERROR,countList);
         }
     }

}
