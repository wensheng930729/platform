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
import com.bee.platform.datadriver.rq.ErpGetOneWarehousingOrderRQ;
import com.bee.platform.datadriver.rq.ErpWarehousingOrderRQ;
import com.bee.platform.datadriver.rq.ErpWarehousingOrderSearchRQ;
import com.bee.platform.datadriver.service.ErpWarehousingOrderService;
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
 * 成品入库主表 前端控制器
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpWarehousingOrder", tags = "成品入库相关接口")
@RequestMapping("/erpWarehousingOrder")
public class ErpWarehousingOrderController {

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private ErpWarehousingOrderService erpWarehousingOrderService;

    @ApiOperation(value = "条件搜索成品入库列表")
    @PostMapping("/searchWarehousingOrderByCondition")
    public ResponseResult<List<ErpWarehousingOrderSearchListDTO>> searchWarehousingOrderByCondition(HttpServletRequest request, @RequestBody ErpWarehousingOrderSearchRQ rq, Page page) {
        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));

        if (ObjectUtils.isEmpty(companyId) && ObjectUtils.isEmpty(rq.getCompanyId())){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        return erpWarehousingOrderService.searchWarehousingOrderByCondition(rq, page, companyId);
    }


    @ApiOperation(value = "根据id查看成品入库详情")
    @GetMapping("/getWarehousingOrderById/{id}")
    public ResponseResult<ErpWarehousingOrderDTO> getWarehousingOrderById(@PathVariable Integer id) {
        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        ErpWarehousingOrderDTO dto = erpWarehousingOrderService.getWarehousingOrderById(id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }


    @Log(businessType = EnumBusinessType.PRODUCT_STOCK, operateType = OperateType.DELETE)
    @ApiOperation(value = "删除成品入库单")
    @DeleteMapping("/deleteWarehousingOrder/{id}")
    public ResponseResult<Integer> deleteWarehousingOrderById(HttpServletRequest request, @PathVariable Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        erpWarehousingOrderService.deleteWarehousingOrderById(userInfo, id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }

    @Log(businessType = EnumBusinessType.PRODUCT_STOCK, operateType = OperateType.EDIT)
    @ApiOperation(value = "修改成品入库单状态")
    @PutMapping("/updateWarehousingOrderState")
    public ResponseResult<Integer> updateWarehousingOrderState(HttpServletRequest request, @RequestParam("id") Integer id, @RequestParam("state") Integer state) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        erpWarehousingOrderService.updateWarehousingOrderState(userInfo, id, state);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }


    @Log(businessType = EnumBusinessType.PRODUCT_STOCK, operateType = OperateType.ADD)
    @ApiOperation(value = "保存成品入库")
    @PostMapping("/saveWarehousingOrder")
    public ResponseResult<Integer> saveWarehousingOrder(HttpServletRequest request, @RequestBody @Valid ErpWarehousingOrderRQ rq) {
        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getId()) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        Integer id = erpWarehousingOrderService.saveWarehousingOrder(userInfo, rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, id);
    }


    @ApiOperation(value = "查询用户下的成品入库列表")
    @GetMapping("/getUserWarehousingOrderList")
    public ResponseResult<List<ErpWarehousingListDTO>> getUserWarehousingOrderList(HttpServletRequest request) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo) || ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        String sysToken = request.getHeader("sysToken");

        List<ErpWarehousingListDTO> dto = erpWarehousingOrderService.getUserWarehousingOrderList(sysToken);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }

    @ApiOperation(value = "化验管理处根据条件获取一条成品入库单号")
    @PostMapping("/getOneWarehousingOrder")
    public ResponseResult<ErpGetOneWarehousingDTO> getOneWarehousingOrder(@RequestBody @Valid ErpGetOneWarehousingOrderRQ rq){

        if (ObjectUtils.isEmpty(rq)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        ErpGetOneWarehousingDTO dto= erpWarehousingOrderService.getOneWarehousingOrder(rq);


        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);
    }



    @ApiOperation(value = "根据料批id查询出料批中产成品id名称和单位")
    @GetMapping("/getProductByMaterialBatchId")
    public ResponseResult<ErpProductBoxDTO> getProductByMaterialBatchId(@RequestParam Integer id) {
        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        ErpProductBoxDTO dto = erpWarehousingOrderService.getProductByMaterialBatchId(id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, dto);

    }


}

