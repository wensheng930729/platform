package com.bee.platform.datadriver.controller;


import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.common.utils.WebUtils;
import com.bee.platform.customer.dto.AuthCustomerBoxDto;
import com.bee.platform.customer.service.AuthCustomerService;
import com.bee.platform.datadriver.dto.ErpReceiptOrderDTO;
import com.bee.platform.datadriver.dto.ErpReceiptOrderSearchDTO;
import com.bee.platform.datadriver.entity.ErpSaleOrder;
import com.bee.platform.datadriver.enums.EnumBusinessType;
import com.bee.platform.datadriver.rq.ErpReceiptOrderRQ;
import com.bee.platform.datadriver.rq.ErpReceiptOrderUpdateRQ;
import com.bee.platform.datadriver.rq.ErpReceiptSearchRQ;
import com.bee.platform.datadriver.service.ErpReceiptOrderService;
import com.bee.platform.datadriver.service.ErpSaleOrderService;
import com.bee.platform.datadriver.support.OperateType;
import com.bee.platform.datadriver.support.annotation.Log;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.util.ObjectUtils;
import org.springframework.web.bind.annotation.*;
import springfox.documentation.annotations.ApiIgnore;

import javax.servlet.http.HttpServletRequest;
import javax.validation.Valid;
import java.util.List;

/**
 * <p>
 * 销售收款单主表 前端控制器
 * </p>
 *
 * @author cheng.ke
 * @since 2019-05-28
 */

@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpReceiptOrder", tags = "销售收款相关接口")
@RequestMapping("/erpReceiptOrder")
public class ErpReceiptOrderController {

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private ErpReceiptOrderService erpReceiptOrderService;

    @Autowired
    private AuthCustomerService authCustomerService;

    @Autowired
    private ErpSaleOrderService erpSaleOrderService;

    @ApiOperation(value = "查询公司下的客户列表")
    @GetMapping("/getCustomerList")
    public ResponseResult<List<AuthCustomerBoxDto>> getCustomerList(Integer orgId){

        AuthPlatformUserInfo userInfo = new AuthPlatformUserInfo().setOrgId(orgId);

        return authCustomerService.getEnterpriseCustomer("", userInfo);
    }

    @ApiOperation(value = "根据公司和客户id查询销售订单列表")
    @GetMapping("/getSaleOrderList")
    public ResponseResult<List<ErpSaleOrder>> getSaleOrderList(@RequestParam("companyId") Integer companyId,@RequestParam("customerId") Integer customerId){

        List<ErpSaleOrder> saleOrders = erpSaleOrderService.selectList(new EntityWrapper<ErpSaleOrder>().eq("company", companyId).eq("customer", customerId).eq("deleted", 0));

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,saleOrders);
    }



    @Log(businessType= EnumBusinessType.SALE_RECEIPT, operateType = OperateType.ADD)
    @ApiOperation(value = "保存销售收款单")
    @PostMapping("/saveReceiptOrder")
    public ResponseResult<Integer> saveReceiptOrder(HttpServletRequest request, @RequestBody @Valid  ErpReceiptOrderRQ rq) {
        if(ObjectUtils.isEmpty(rq)){
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getId())||ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        Integer id = erpReceiptOrderService.saveReceiptOrder(userInfo,rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,id);
    }

    @Log(businessType= EnumBusinessType.SALE_RECEIPT, operateType = OperateType.EDIT)
    @ApiOperation(value = "编辑销售收款单")
    @PostMapping("updateReceiptOrder")
    @ApiIgnore
    public ResponseResult<Integer> updateReceiptOrder(HttpServletRequest request, @RequestBody @Valid ErpReceiptOrderUpdateRQ rq){

        if(ObjectUtils.isEmpty(rq)){
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }


        Integer id=  erpReceiptOrderService.updateReceiptOrder(userInfo,rq);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,id);

    }




    @ApiOperation(value = "查询销售收款单")
    @GetMapping("/getReceiptOrder/{id}")
    public ResponseResult<ErpReceiptOrderDTO> getReceiptOrderById(@PathVariable  Integer id) {
        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
         ErpReceiptOrderDTO dto = erpReceiptOrderService.getReceiptOrderById(id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);
    }

    @Log(businessType= EnumBusinessType.SALE_RECEIPT, operateType = OperateType.EDIT)
    @ApiOperation(value = "修改销售收款确认状态")
    @PutMapping("/updateReceiptOrderState")
    public ResponseResult<Integer> updateReceiptOrderState(HttpServletRequest request, @RequestParam("id")  Integer id, @RequestParam("state") Integer state) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        erpReceiptOrderService.updateReceiptOrderState(userInfo,id,state);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,id);
    }

    @ApiOperation(value = "条件搜索销售收款列表")
    @PostMapping("/searchReceiptByCondition")
    public ResponseResult<List<ErpReceiptOrderSearchDTO>> searchReceiptByCondition(HttpServletRequest request, @RequestBody ErpReceiptSearchRQ rq, Page page) {
        if(ObjectUtils.isEmpty(rq)){
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        Integer companyId = Integer.valueOf(WebUtils.getParam(ConstantsUtil.COMPANY, request));

        if (ObjectUtils.isEmpty(companyId) && ObjectUtils.isEmpty(rq.getCompanyId())){
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        return erpReceiptOrderService.searchReceiptByCondition(rq,page,companyId);
    }

    @Log(businessType= EnumBusinessType.SALE_RECEIPT, operateType = OperateType.DELETE)
    @ApiOperation(value = "删除销售收款单")
    @DeleteMapping("/deleteReceiptOrderById")
    public ResponseResult<Integer> deleteReceiptOrderById(HttpServletRequest request, @RequestParam("id") Integer id){

        if(ObjectUtils.isEmpty(id)){
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);
        if (ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }

        erpReceiptOrderService.deleteReceiptOrderById(userInfo,id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,id);

    }


}

