package com.bee.platform.datadriver.controller;

import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.service.CommonService;
import com.bee.platform.user.authority.dto.AuthPlatformUserPullDownDto;
import com.bee.platform.user.authority.dto.AuthResourceDetailDTO;
import com.bee.platform.user.service.feign.AuthResourcesFeignClient;
import com.bee.platform.user.service.feign.UserInfoFeignClient;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import java.util.List;


/**
 * @author dell
 * @version 1.0.0
 * @ClassName CommonController
 * @Description 功能描述
 * @Date 2019/6/2 10:24
 **/


@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "commonMethod", tags = "公共方法相关接口")
@RequestMapping("/commonMethod")
public class CommonController {


    @Autowired
    private CommonService commonService;

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private AuthResourcesFeignClient resourcesFeignClient;

    @Autowired
    private UserInfoFeignClient userInfoFeignClient;

    @ApiOperation(value = "根据业务类型生成订单号", notes = "根据业务类型生成订单号")
    @GetMapping("/generateOrderId")
    public ResponseResult generateOrderId(@RequestParam Integer type) {

        if (ObjectUtils.isEmpty(type)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        String code = commonService.generateOrderId(type);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, (Object)code);

    }


    @ApiOperation(value = "根据公司id查询仓库列表", notes = "根据公司id查询仓库列表")
    @GetMapping("/getRepositoryListByCompanyId")
    public ResponseResult<List<ErpRepositoryListDTO>> getRepositoryListByCompanyId(@RequestParam Integer companyId) {

        if (ObjectUtils.isEmpty(companyId)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        List<ErpRepositoryListDTO> dto = commonService.getRepositoryListByCompanyId(companyId);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }


    @ApiOperation(value = "根据公司id查询公司下的产品列表", notes = "根据公司id查询公司下的产品列表")
    @GetMapping("/getProductListByCompanyId")
    public ResponseResult<List<ErpProductBoxDTO>> getProductListByCompanyId(@RequestParam Integer companyId) {

        if (ObjectUtils.isEmpty(companyId)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        List<ErpProductBoxDTO> dto = commonService.getProductListByCompanyId(companyId);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }


    @ApiOperation(value = "根据公司id查询公司下的炉号列表", notes = "根据公司id查询公司下的炉号列表")
    @GetMapping("/getFurnaceListByCompanyId")
    public ResponseResult<List<ErpFurnaceBoxDTO>> getFurnaceListByCompanyId(@RequestParam Integer companyId) {

        if (ObjectUtils.isEmpty(companyId)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        List<ErpFurnaceBoxDTO> dto = commonService.getFurnaceListByCompanyId(companyId);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }


    @ApiOperation(value = "模糊搜索化验单列表", notes = "模糊搜索化验单列表")
    @GetMapping("/searchTestReportList")
    public ResponseResult<List<ErpTestReportSearchListDTO>> searchTestReportList(@RequestParam String testCode) {

        if (ObjectUtils.isEmpty(testCode)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        List<ErpTestReportSearchListDTO> dto = commonService.searchTestReportList(testCode);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }


    @ApiOperation(value = "公司Id加模糊搜索化验单列表", notes = "模糊搜索化验单列表")
    @GetMapping("/searchTestReportListAddCompanyId")
    public ResponseResult<List<ErpTestReportSearchListDTO>> searchTestReportListAddCompanyId(@RequestParam String testCode, @RequestParam Integer companyId) {

        if (ObjectUtils.isEmpty(testCode)||ObjectUtils.isEmpty(companyId)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        List<ErpTestReportSearchListDTO> dto = commonService.searchTestReportListAddCompanyId(testCode, companyId);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }



    @ApiOperation(value = "产品Id加模糊搜索化验单列表", notes = "产品Id加模糊搜索化验单列表")
    @GetMapping("/searchTestReportListAddProductId")
    public ResponseResult<List<ErpTestReportSearchByConditionDTO>> searchTestReportListAddProductId(@RequestParam String testCode, @RequestParam Integer productId) {

        if (ObjectUtils.isEmpty(testCode)||ObjectUtils.isEmpty(productId)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        List<ErpTestReportSearchByConditionDTO> dto = commonService.searchTestReportListAddProductId(testCode, productId);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }

    @ApiOperation(value = "产品Id加公司id搜索化验单列表", notes = "产品Id加公司id搜索化验单列表")
    @GetMapping("/searchTestReportListByProductIdAndCompanyId")
    public ResponseResult<List<ErpTestReportSearchByConditionDTO>> searchTestReportListByProductIdAndCompanyId(@RequestParam Integer companyId, @RequestParam Integer productId) {

        if (ObjectUtils.isEmpty(companyId)||ObjectUtils.isEmpty(productId)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        List<ErpTestReportSearchByConditionDTO> dto = commonService.searchTestReportListByProductIdAndCompanyId(companyId, productId);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }



    @ApiOperation(value = "根据登录人查询其公司及其子公司采购订单列表", notes = "根据登录人查询其公司及其子公司采购订单列表")
    @GetMapping("/getPurchaseListByUserInfo")
    public ResponseResult<List<ErpPurchaseListDTO>> getPurchaseListByUserInfo(HttpServletRequest request) {

        String sysToken = request.getHeader("sysToken");

        List<ErpPurchaseListDTO> dto = commonService.getPurchaseListByUserInfo(sysToken);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }


    @ApiOperation(value = "根据登录人公司id查询采购订单列表", notes = "根据登录人公司id查询采购订单列表")
    @GetMapping("/getPurchaseListByUserCompanyId")
    public ResponseResult<List<ErpPurchaseListDTO>> getPurchaseListByUserCompanyId(HttpServletRequest request) {

        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));

        if (ObjectUtils.isEmpty(companyId)){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        List<ErpPurchaseListDTO> dto = commonService.getPurchaseListByUserCompanyId(companyId);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }


    @ApiOperation(value = "根据登录人公司id查询销售订单列表", notes = "根据登录人公司id查询销售订单列表")
    @GetMapping("/getSaleListByUserCompanyId")
    public ResponseResult<List<ErpSaleListDTO>> getSaleListByUserCompanyId(HttpServletRequest request) {

        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));

        if (ObjectUtils.isEmpty(companyId)){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        List<ErpSaleListDTO> dto = commonService.getSaleListByUserCompanyId(companyId);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }

    @ApiOperation(value = "根据采购订单id查询采购订单详情的产品列表", notes = "根据采购订单id查询采购订单详情的产品列表")
    @GetMapping("/getProductListByPurchaseId")
    public ResponseResult<List<ErpProductBoxDTO>> getProductListByPurchaseId(@RequestParam Integer purchaseId) {
        if (ObjectUtils.isEmpty(purchaseId)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        List<ErpProductBoxDTO> dto = commonService.getProductListByPurchaseId(purchaseId);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }


    @ApiOperation(value = "根据销售订单id查询销售订单详情的产品列表", notes = "根据销售订单id查询销售订单详情的产品列表")
    @GetMapping("/getProductListBySaleId")
    public ResponseResult<List<ErpProductBoxDTO>> getProductListBySaleId(@RequestParam Integer saleId) {
        if (ObjectUtils.isEmpty(saleId)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        List<ErpProductBoxDTO> dto = commonService.getProductListBySaleId(saleId);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }

    @GetMapping("/resourcesByDriver")
    @ApiOperation(value = "获得资源-录入系统")
    public ResponseResult<List<AuthResourceDetailDTO>> listResourcesByUser(HttpServletRequest request,
                                                                           @RequestParam String subSys, @RequestParam(required = false) String sysToken){
        return resourcesFeignClient.listResourcesByUser(subSys,sysToken);
    }

    @GetMapping("/companyUserById")
    @ApiOperation(value = "查询当前用户公司所有人员-下拉列表")
    public ResponseResult<List<AuthPlatformUserPullDownDto>> getAllCompanyUserById(HttpServletRequest request){
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        return userInfoFeignClient.getAllCompanyUserById(userInfo.getOrgId());
    }


}
