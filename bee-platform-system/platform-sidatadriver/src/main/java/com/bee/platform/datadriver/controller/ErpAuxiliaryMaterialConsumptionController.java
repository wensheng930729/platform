package com.bee.platform.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.datadriver.dto.ErpAuxiliaryMaterialConsumptionDTO;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.ErpAuxiliaryMaterialConsumptionRQ;
import com.bee.platform.datadriver.rq.ErpAuxiliaryMaterialConsumptionSearchRQ;
import com.bee.platform.datadriver.service.ErpAuxiliaryMaterialConsumptionService;
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
import java.util.Objects;

/**
 * <p>
 * 辅材消耗表 前端控制器
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpAuxiliaryMaterialConsumption", tags = "辅材消耗相关接口")
@RequestMapping("/erpAuxiliaryMaterialConsumption")
public class ErpAuxiliaryMaterialConsumptionController {

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private ErpAuxiliaryMaterialConsumptionService erpAuxiliaryMaterialConsumptionService;

    @ApiOperation(value = "条件搜索辅材消耗列表")
    @PostMapping("/searchAuxiliaryMaterialConsumptionByCondition")
    public ResponseResult<List<ErpAuxiliaryMaterialConsumptionDTO>> searchAuxiliaryMaterialConsumptionByCondition(HttpServletRequest request, @RequestBody ErpAuxiliaryMaterialConsumptionSearchRQ rq, Page page) {
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

        if (ObjectUtils.isEmpty(companyId) && ObjectUtils.isEmpty(rq.getCompanyId())){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        return erpAuxiliaryMaterialConsumptionService.searchAuxiliaryMaterialConsumptionByCondition(rq, page, companyId);
    }


    @ApiOperation(value = "保存辅材消耗信息")
    @PostMapping("saveAuxiliaryMaterialConsumption")
    public ResponseResult<Integer> saveAuxiliaryMaterialConsumption(HttpServletRequest request, @RequestBody @Valid ErpAuxiliaryMaterialConsumptionRQ rq) {

        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id = erpAuxiliaryMaterialConsumptionService.saveAuxiliaryMaterialConsumption(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);

    }


    @Log(businessType = EnumBusinessType.AUXILIARY_MATERIAL_CONSUMPTION, operateType = OperateType.DELETE)
    @ApiOperation(value = "根据id删除辅材消耗")
    @DeleteMapping("/deleteAuxiliaryMaterialConsumptionById/{id}")
    public ResponseResult<Integer> deleteAuxiliaryMaterialConsumptionById(HttpServletRequest request, @PathVariable Integer id) {
        if (ObjectUtils.isEmpty(id)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        erpAuxiliaryMaterialConsumptionService.deleteAuxiliaryMaterialConsumptionById(userInfo, id);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }


    @ApiOperation(value = "根据id查看辅材消耗")
    @GetMapping("/getAuxiliaryMaterialConsumptionById/{id}")
    public ResponseResult<ErpAuxiliaryMaterialConsumptionDTO> getAuxiliaryMaterialConsumptionById(@PathVariable Integer id) {
        if (ObjectUtils.isEmpty(id)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        ErpAuxiliaryMaterialConsumptionDTO dto = erpAuxiliaryMaterialConsumptionService.getAuxiliaryMaterialConsumptionById(id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }


}

