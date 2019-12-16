package com.bee.platform.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.dto.*;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.*;
import com.bee.platform.datadriver.service.ErpRepositoryReceiptService;
import com.bee.platform.datadriver.support.OperateType;
import com.bee.platform.datadriver.support.annotation.Log;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 *  前端控制器
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-27
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@RequestMapping("/erpRepositoryReceipt")
@Api(value = "原料入库(采购收货)和成品出库(销售发货)操作相关接口", tags = "原料入库(采购收货)和成品出库(销售发货)操作相关接口")
public class ErpRepositoryReceiptController {

    @Autowired
    private ErpRepositoryReceiptService repositoryReceiptService;
    @Autowired
    private UserInfoUtils userInfoUtils;

    @ApiOperation(value = "条件搜索库存流水帐")
    @PostMapping("/searchInventoryFlowByCondition")
    public ResponseResult<List<ErpInventoryFlowDTO>> searchInventoryFlowByCondition(HttpServletRequest request,@RequestBody ErpInventoryFlowSearchRQ rq, Page page){

        if(ObjectUtils.isEmpty(rq)){
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Integer companyId = userInfo.getOrgId();

        if (ObjectUtils.isEmpty(companyId) && ObjectUtils.isEmpty(rq.getCompanyId())){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        return  repositoryReceiptService.searchInventoryFlowByCondition(rq,page,companyId);


    }



    @ApiOperation(value = "条件搜索原料入库(采购收货)")
    @PostMapping("/searchRepositoryReceiptRawInByCondition")
    public ResponseResult<List<ErpRepositoryReceiptRawInSearchDTO>> searchRepositoryReceiptRawInByCondition(HttpServletRequest request,@RequestBody ErpRepositoryReceiptRawInSearchRQ rq, Page page){

        if(ObjectUtils.isEmpty(rq)){
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Integer companyId = userInfo.getOrgId();

        if (ObjectUtils.isEmpty(companyId) && ObjectUtils.isEmpty(rq.getCompanyId())){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        return  repositoryReceiptService.searchRepositoryReceiptRawInByCondition(rq,page,companyId);


    }


    @ApiOperation(value = "条件搜索成品出库(销售发货)")
    @PostMapping("/searchRepositoryReceiptProductOutByCondition")
    public ResponseResult<List<ErpRepositoryReceiptProductOutSearchDTO>> searchRepositoryReceiptProductOutByCondition(HttpServletRequest request,@RequestBody ErpRepositoryReceiptProductOutSearchRQ rq, Page page){

        if(ObjectUtils.isEmpty(rq)){
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Integer companyId = userInfo.getOrgId();

        if (ObjectUtils.isEmpty(companyId) && ObjectUtils.isEmpty(rq.getCompanyId())){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        return  repositoryReceiptService.searchRepositoryReceiptProductOutByCondition(rq,page,companyId);


    }





    @ApiOperation(value = "保存原料入库主单(采购收货)")
    @PostMapping("/saveRepositoryReceiptRawIn")
    public ResponseResult<Integer> saveRepositoryReceiptRawIn(HttpServletRequest request, @RequestBody @Valid RepositoryReceiptRawInRQ rq) {
        if(ObjectUtils.isEmpty(rq)){
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getId())||ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        Integer id = repositoryReceiptService.saveRepositoryReceiptRawIn(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,id);
    }



    @Log(businessType= EnumBusinessType.MATERIAL_STOCK, operateType = OperateType.EDIT)
    @ApiOperation(value = "修改原料入库单状态(采购收货)")
    @PutMapping("/updateRepositoryReceiptRawInState")
    public ResponseResult<Integer> updateRepositoryReceiptRawInState(HttpServletRequest request,@RequestParam("id")  Integer id,@RequestParam("state") Integer state) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        repositoryReceiptService.updateRepositoryReceiptRawInState(userInfo,id,state);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,id);
    }



    @Log(businessType= EnumBusinessType.MATERIAL_STOCK, operateType = OperateType.DELETE)
    @ApiOperation(value = "删除原料入库单(采购收货)")
    @DeleteMapping("/deleteRepositoryReceiptRawInById/{id}")
    public ResponseResult<Integer> deleteRepositoryReceiptRawInById(HttpServletRequest request,@PathVariable  Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        repositoryReceiptService.deleteRepositoryReceiptRawInById(userInfo,id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,id);
    }


    @ApiOperation(value = "根据id查看原料入库详情(采购收货)")
    @GetMapping("/getRepositoryReceiptRawInById/{id}")
    public ResponseResult<ErpRepositoryReceiptRawInDTO> getRepositoryReceiptRawInById(@PathVariable  Integer id){
        if (ObjectUtils.isEmpty(id)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        ErpRepositoryReceiptRawInDTO dto =  repositoryReceiptService.getRepositoryReceiptRawInById(id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);

    }


    /**
     * 保存成品出库
     * @param request
     * @param rq
     * @return
     */
    @ApiOperation(value = "保存成品出库主单(销售发货)")
    @PostMapping("/saveRepositoryReceiptProductOut")
    public ResponseResult<Integer> saveRepositoryReceiptProductOut(HttpServletRequest request, @RequestBody @Valid ErpRepositoryReceiptProductOutRQ rq) {
        if(ObjectUtils.isEmpty(rq)){
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getId())||ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        Integer id = repositoryReceiptService.saveRepositoryReceiptProductOut(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,id);
    }


    /**
     * 修改成品出库单状态
     * @param request
     * @param id
     * @param state
     * @return
     */
    @Log(businessType= EnumBusinessType.PRODUCT_DELIVERY, operateType = OperateType.EDIT)
    @ApiOperation(value = "修改成品出库单状态(销售发货)")
    @PutMapping("/updateRepositoryReceiptProductOutState")
    public ResponseResult<Integer> updateRepositoryReceiptProductOutState(HttpServletRequest request,@RequestParam("id")  Integer id,@RequestParam("state") Integer state) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        repositoryReceiptService.updateRepositoryReceiptProductOutState(userInfo,id,state);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,id);
    }



    @Log(businessType= EnumBusinessType.PRODUCT_DELIVERY, operateType = OperateType.DELETE)
    @ApiOperation(value = "删除成品出库单(销售发货)")
    @DeleteMapping("/deleteRepositoryReceiptProductOutById/{id}")
    public ResponseResult<Integer> deleteRepositoryReceiptProductOutById(HttpServletRequest request,@PathVariable Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        repositoryReceiptService.deleteRepositoryReceiptProductOutById(userInfo,id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,id);
    }



    @ApiOperation(value = "根据id查看成品出库详情(销售发货)")
    @GetMapping("/getRepositoryReceiptProductOutById/{id}")
    public ResponseResult<ErpRepositoryReceiptProductOutDTO> getRepositoryReceiptProductOutById(@PathVariable  Integer id){
        if (ObjectUtils.isEmpty(id)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        ErpRepositoryReceiptProductOutDTO dto =  repositoryReceiptService.getRepositoryReceiptProductOutById(id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);

    }

    @ApiOperation(value = "根据销售订单id查看销售发货情况")
    @GetMapping("/getSaleDeliveryInfo/{id}")
    public ResponseResult<List<RepoReceiptDetailDTO>> getSaleDeliveryInfo(@PathVariable  Integer id){
        if (ObjectUtils.isEmpty(id)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return repositoryReceiptService.getSaleDeliveryInfo(id);
    }

}

