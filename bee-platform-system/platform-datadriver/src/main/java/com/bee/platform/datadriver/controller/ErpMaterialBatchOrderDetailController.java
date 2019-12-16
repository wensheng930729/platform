package com.bee.platform.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.datadriver.dto.ErpMaterialBatchDetailsDTO;
import com.bee.platform.datadriver.dto.ErpMaterialBatchOrderDetailDTO;
import com.bee.platform.datadriver.rq.ErpMaterialBatchOrderDetailRQ;
import com.bee.platform.datadriver.service.ErpMaterialBatchOrderDetailService;
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
 * 料批明细表 前端控制器
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpMaterialBatchOrderDetail", tags = "料批详情相关接口")
@RequestMapping("/erpMaterialBatchOrderDetail")
public class ErpMaterialBatchOrderDetailController {

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private ErpMaterialBatchOrderDetailService erpMaterialBatchOrderDetailService;


    @ApiOperation(value = "保存料批详情信息单")
    @PostMapping("/saveMaterialBatchOrderDetail")
    public ResponseResult<Integer> saveMaterialBatchOrderDetail(HttpServletRequest request, @RequestBody @Valid ErpMaterialBatchOrderDetailRQ rq) {
        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId()) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        Integer id = erpMaterialBatchOrderDetailService.saveMaterialBatchOrderDetail(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }

    @ApiOperation(value = "根据id删除料批详情")
    @DeleteMapping("/deleteMaterialBatchOrderDetailById/{id}")
    public ResponseResult<Integer> deleteMaterialBatchOrderDetailById(HttpServletRequest request, @PathVariable Integer id) {
        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId()) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        erpMaterialBatchOrderDetailService.deleteMaterialBatchOrderDetailById(userInfo, id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }


    @ApiOperation(value = "根据料批id查询明细产品名称和单位")
    @GetMapping("/getMaterialBatchDetailsProduct")
    public ResponseResult<List<ErpMaterialBatchDetailsDTO>> getMaterialBatchDetails(@RequestParam Integer id) {
        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        List<ErpMaterialBatchDetailsDTO> dto = erpMaterialBatchOrderDetailService.getMaterialBatchDetails(id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


    @ApiOperation(value = "根据id查询料批明细详情")
    @GetMapping("getMaterialBatchDetailById")
    public ResponseResult<ErpMaterialBatchOrderDetailDTO> getMaterialBatchDetailById(@RequestParam Integer id) {
        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        ErpMaterialBatchOrderDetailDTO dto = erpMaterialBatchOrderDetailService.getMaterialBatchDetailById(id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }


}

