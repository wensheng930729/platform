package com.bee.platform.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.datadriver.dto.ErpOutOfStockOrderDTO;
import com.bee.platform.datadriver.dto.ErpOutOfStockOrderSearchListDTO;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.ErpOutOfStockOrderRQ;
import com.bee.platform.datadriver.rq.ErpOutOfStockSearchRQ;
import com.bee.platform.datadriver.service.ErpOutOfStockOrderService;
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
 * 领料出库主表 前端控制器
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpOutOfStockOrder", tags = "领料出库相关接口")
@RequestMapping("/erpOutOfStockOrder")
public class ErpOutOfStockOrderController {

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private ErpOutOfStockOrderService erpOutOfStockOrderService;

    @ApiOperation(value = "条件搜索领料出库")
    @PostMapping("/searchOutOfStockOrderByCondition")
    public ResponseResult<List<ErpOutOfStockOrderSearchListDTO>> searchOutOfStockOrderByCondition(HttpServletRequest request, @RequestBody ErpOutOfStockSearchRQ rq, Page page) {
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

        return erpOutOfStockOrderService.searchOutOfStockOrderByCondition(rq, page, companyId);
    }


    @ApiOperation(value = "根据id查看领料出库详情")
    @GetMapping("/getOutOfStockOrderById/{id}")
    public ResponseResult<ErpOutOfStockOrderDTO> getOutOfStockOrderById(@PathVariable Integer id) {
        if (ObjectUtils.isEmpty(id)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        ErpOutOfStockOrderDTO dto = erpOutOfStockOrderService.getOutOfStockOrderById(id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }


    @Log(businessType = EnumBusinessType.MATERIAL_REQUISITION, operateType = OperateType.DELETE)
    @ApiOperation(value = "删除领料出库单")
    @DeleteMapping("/deleteOutOfStockOrder/{id}")
    public ResponseResult<Integer> deleteOutOfStockOrderById(HttpServletRequest request, @PathVariable Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        erpOutOfStockOrderService.deleteOutOfStockOrderById(userInfo, id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }

    @Log(businessType = EnumBusinessType.MATERIAL_REQUISITION, operateType = OperateType.EDIT)
    @ApiOperation(value = "修改领料出库单状态")
    @PutMapping("/updateOutOfStockOrderState")
    public ResponseResult<Integer> updateOutOfStockOrderState(HttpServletRequest request, @RequestParam("id") Integer id, @RequestParam("state") Integer state) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        erpOutOfStockOrderService.updateOutOfStockOrderState(userInfo, id, state);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }

    @ApiOperation(value = "保存领料出库单")
    @PostMapping("/saveOutOfStockOrder")
    public ResponseResult<Integer> saveOutOfStockOrder(HttpServletRequest request, @RequestBody @Valid ErpOutOfStockOrderRQ rq) {
        if (ObjectUtils.isEmpty(rq)) {
            log.info("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId()) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.info("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        Integer id = erpOutOfStockOrderService.saveOutOfStockOrder(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }


}

