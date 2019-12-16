package com.bee.platform.dinas.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.dinas.datadriver.dto.*;
import com.bee.platform.dinas.datadriver.enums.EnumBusinessType;
import com.bee.platform.dinas.datadriver.rq.*;
import com.bee.platform.dinas.datadriver.service.DinasInspectionGoodsService;
import com.bee.platform.dinas.datadriver.support.OperateType;
import com.bee.platform.dinas.datadriver.support.annotation.Log;
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
 * 验货磅单表 前端控制器
 * </p>
 *
 * @author liliang123
 * @since 2019-08-13
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "dinasInspectionGoods", tags = "砂石验货磅单表相关的接口")
@RequestMapping("/dinasInspectionGoods")
public class DinasInspectionGoodsController {


    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private DinasInspectionGoodsService dinasInspectionGoodsService;


    @ApiOperation(value = "条件搜索砂石验货磅单列表")
    @PostMapping("/searchInspectionGoodsByCondition")
    public ResponseResult<List<DinasInspectionGoodsSearchDTO>> searchInspectionGoodsByCondition(HttpServletRequest request, @RequestBody DinasInspectionGoodsSearchRQ rq, Page page) {
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Integer companyId = userInfo.getOrgId();
        if (ObjectUtils.isEmpty(companyId)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        return dinasInspectionGoodsService.searchInspectionGoodsByCondition(rq, page, companyId);
    }

    @Log(businessType = EnumBusinessType.INSPECTION_GOODS, operateType = OperateType.ADD)
    @ApiOperation(value = "保存验货磅单信息")
    @PostMapping("saveInspectionGoods")
    public ResponseResult<Integer> saveInspectionGoods(HttpServletRequest request, @RequestBody @Valid DinasInspectionGoodsSaveRQ rq) {

        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Integer id = dinasInspectionGoodsService.saveInspectionGoods(userInfo, rq);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }

    @Log(businessType = EnumBusinessType.INSPECTION_GOODS, operateType = OperateType.EDIT)
    @ApiOperation(value = "编辑验货磅单信息")
    @PostMapping("updateInspectionGoods")
    public ResponseResult<Integer> updateInspectionGoods(HttpServletRequest request, @RequestBody @Valid DinasInspectionGoodsUpdateRQ rq) {

        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Integer id = dinasInspectionGoodsService.updateInspectionGoods(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }

    @ApiOperation(value = "根据id查看验货磅单")
    @GetMapping("/getInspectionGoodsById/{id}")
    public ResponseResult<DinasInspectionGoodsDTO> getInspectionGoodsById(@PathVariable Integer id) {
        if (ObjectUtils.isEmpty(id)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        DinasInspectionGoodsDTO dto = dinasInspectionGoodsService.getInspectionGoodsById(id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }


    @Log(businessType = EnumBusinessType.INSPECTION_GOODS, operateType = OperateType.DELETE, isBatch = true)
    @ApiOperation(value = "根据ids删除验货磅单")
    @PostMapping("/deleteInspectionGoodsByIds")
    public ResponseResult<List<Integer>> deleteInspectionGoodsByIds(HttpServletRequest request, @RequestBody @Valid DeletedBatchRQ rq) {
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        List<Integer> ids = dinasInspectionGoodsService.deleteInspectionGoodsByIds(userInfo, rq.getIds());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, ids);
    }

    @ApiOperation(value = "根据登录人公司信息查询采购合同列表")
    @GetMapping("/getPurchaseCodeList")
    public ResponseResult<List<DinasPurchaseCodeListDTO>> getPurchaseCodeList(HttpServletRequest request) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Integer companyId = userInfo.getOrgId();

        if (ObjectUtils.isEmpty(companyId)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        List<DinasPurchaseCodeListDTO> dto = dinasInspectionGoodsService.getPurchaseCodeList(companyId);


        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


    @ApiOperation(value = "根据登录人公司信息查询销售合同列表")
    @GetMapping("/getSaleCodeList")
    public ResponseResult<List<DinasSaleCodeListDTO>> getSaleCodeList(HttpServletRequest request) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Integer companyId = userInfo.getOrgId();

        if (ObjectUtils.isEmpty(companyId)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        List<DinasSaleCodeListDTO> dto = dinasInspectionGoodsService.getSaleCodeList(companyId);


        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


    @ApiOperation(value = "根据采购合同id和销售合同id查询产品列表")
    @PostMapping("/getProductList")
    public ResponseResult<List<DinasProductListDTO>> getProductListByPurchaseIdAndSaleId(@RequestBody @Valid DinasGetProductListRQ rq) {

        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        List<DinasProductListDTO> dto = dinasInspectionGoodsService.getProductListByPurchaseIdAndSaleId(rq);


        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


    @ApiOperation(value = "根据采购合同id和销售合同id和产品id查询产品批次列表")
    @PostMapping("/getProductSpecList")
    public ResponseResult<List<DinasProductSpecListDTO>> getProductSpecListByPurchaseIdAndSaleIdAndProductId(@RequestBody @Valid DinasGetProductSpecListRQ rq) {

        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        List<DinasProductSpecListDTO> dto = dinasInspectionGoodsService.getProductSpecListByPurchaseIdAndSaleIdAndProductId(rq);


        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


}

