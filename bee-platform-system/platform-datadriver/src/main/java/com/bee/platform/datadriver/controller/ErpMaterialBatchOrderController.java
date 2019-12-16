package com.bee.platform.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.dto.ErpMaterialBatchListDTO;
import com.bee.platform.datadriver.dto.ErpMaterialBatchOrderDTO;
import com.bee.platform.datadriver.dto.ErpMaterialBatchSearchListDTO;
import com.bee.platform.datadriver.dto.ErpProductBoxDTO;
import com.bee.platform.datadriver.entity.ErpMaterialBatchOrder;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.ErpMaterialBatchOrderRQ;
import com.bee.platform.datadriver.rq.ErpMaterialBatchSearchRQ;
import com.bee.platform.datadriver.service.ErpMaterialBatchOrderService;
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
 * 料批主表 前端控制器
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpMaterialBatchOrder", tags = "料批相关接口")
@RequestMapping("/erpMaterialBatchOrder")
public class ErpMaterialBatchOrderController {
    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private ErpMaterialBatchOrderService erpMaterialBatchOrderService;

    @ApiOperation(value = "条件搜索料批列表")
    @PostMapping("/searchMaterialBatchByCondition")
    public ResponseResult<List<ErpMaterialBatchSearchListDTO>> searchMaterialBatchByCondition(HttpServletRequest request, @RequestBody ErpMaterialBatchSearchRQ rq, Page page) {
        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));

        if (ObjectUtils.isEmpty(companyId) && ObjectUtils.isEmpty(rq.getCompanyId())){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        return erpMaterialBatchOrderService.searchMaterialBatchByCondition( rq, page, companyId);
    }

    @ApiOperation(value = "根据id查看料批详情")
    @GetMapping("/getMaterialBatchById/{id}")
    public ResponseResult<ErpMaterialBatchOrderDTO> getMaterialBatchById(@PathVariable Integer id) {
        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        ErpMaterialBatchOrderDTO dto = erpMaterialBatchOrderService.getMaterialBatchById(id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }


    @Log(businessType = EnumBusinessType.MATERIAL_BATCH, operateType = OperateType.EDIT)
    @ApiOperation(value = "修改料批状态")
    @PutMapping("/updateMaterialBatchState")
    public ResponseResult<Integer> updateMaterialBatchState(HttpServletRequest request, @RequestParam Integer id, @RequestParam Integer state) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        erpMaterialBatchOrderService.updateMaterialBatchState(userInfo, id, state);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }

    @Log(businessType = EnumBusinessType.MATERIAL_BATCH, operateType = OperateType.DELETE)
    @ApiOperation(value = "删除料批")
    @DeleteMapping("/deleteMaterialBatch/{id}")
    public ResponseResult<Integer> deleteMaterialBatchById(HttpServletRequest request, @PathVariable Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        erpMaterialBatchOrderService.deleteMaterialBatchById(userInfo, id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }


    @Log(businessType = EnumBusinessType.MATERIAL_BATCH, operateType = OperateType.ADD)
    @ApiOperation(value = "保存料批单")
    @PostMapping("/saveMaterialBatchOrder")
    public ResponseResult<Integer> saveMaterialBatchOrder(HttpServletRequest request, @RequestBody @Valid ErpMaterialBatchOrderRQ rq) {
        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId()) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        Integer id = erpMaterialBatchOrderService.saveMaterialBatchOrder(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }

    @ApiOperation(value = "根据企业id和产品id查询料批")
    @PostMapping("/getMaterialBatch")
    public ResponseResult<List<ErpMaterialBatchOrder>> getMaterialBatch(@RequestParam Integer productId, @RequestParam Integer companyId) {
        if (ObjectUtils.isEmpty(companyId) || ObjectUtils.isEmpty(productId)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        return erpMaterialBatchOrderService.getMaterialBatch(productId, companyId);

    }


    @ApiOperation(value = "查询当前登录用户及其子企业的料批信息")
    @GetMapping("/getMaterialBatchList")
    public ResponseResult<List<ErpMaterialBatchListDTO>> getMaterialBatchList(HttpServletRequest request) {

        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        String sysToken = request.getHeader("sysToken");
        List<ErpMaterialBatchListDTO> dto = erpMaterialBatchOrderService.getMaterialBatchList(userInfo, sysToken);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }


    @ApiOperation(value = "查询公司id查询公司下的料批信息")
    @GetMapping("/getMaterialBatchListByCompanyId")
    public ResponseResult<List<ErpMaterialBatchListDTO>> getMaterialBatchListByCompanyId(@RequestParam() Integer companyId) {

        if (ObjectUtils.isEmpty(companyId)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        List<ErpMaterialBatchListDTO> dto = erpMaterialBatchOrderService.getMaterialBatchListByCompanyId(companyId);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }

    @ApiOperation(value = "根据料批id查询料批明细产品列表")
    @GetMapping("/getMaterialBatchDetailProductList")
    public ResponseResult<List<ErpProductBoxDTO>> getMaterialBatchDetailProductList(@RequestParam Integer id) {
        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        List<ErpProductBoxDTO> dto = erpMaterialBatchOrderService.getMaterialBatchDetailProductList(id);


        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }


}

