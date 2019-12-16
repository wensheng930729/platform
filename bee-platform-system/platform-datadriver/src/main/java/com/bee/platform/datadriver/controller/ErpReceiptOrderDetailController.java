package com.bee.platform.datadriver.controller;


import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.utils.UserInfoUtils;
import com.bee.platform.datadriver.dto.ErpReceiptOrderDetailDTO;
import com.bee.platform.datadriver.rq.ErpReceiptOrderDetailRQ;
import com.bee.platform.datadriver.service.ErpReceiptOrderDetailService;
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
 * 销售收款单收款详情表 前端控制器
 * </p>
 *
 * @author cheng.ke123
 * @since 2019-05-28
 */
@Slf4j
@RestController
@CrossOrigin(origins = "*")
@Api(value = "erpReceiptOrderDetail", tags = "销售收款详情相关接口")
@RequestMapping("/erpReceiptOrderDetail")
public class ErpReceiptOrderDetailController {

    @Autowired
    private UserInfoUtils userInfoUtils;

    @Autowired
    private ErpReceiptOrderDetailService erpReceiptOrderDetailService;

    @ApiOperation(value = "根据id删除销售收款详情单")
    @DeleteMapping("/deleteById")
    @ApiIgnore
    public ResponseResult<Integer> deleteById(HttpServletRequest request,@RequestParam() Integer id) {
        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);

        if (ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getId())||ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        erpReceiptOrderDetailService.deleteDetail(userInfo,id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,id);
    }

    @ApiOperation(value = "保存销售收款详情信息")
    @PostMapping("/saveReceiptOrderDetail")
    public ResponseResult<Integer> saveReceiptOrderDetail(HttpServletRequest request,@RequestBody @Valid ErpReceiptOrderDetailRQ rq){
        if(ObjectUtils.isEmpty(rq)){
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }

        AuthPlatformUserInfo userInfo = userInfoUtils.getSimpleUserInfo(request);

        if (ObjectUtils.isEmpty(userInfo)||ObjectUtils.isEmpty(userInfo.getId())||ObjectUtils.isEmpty(userInfo.getOrgId())) {
            log.error("无法获取用户信息");
            return ResponseResult.buildResponseResult(ResCodeEnum.FAILED_TO_GET_USER_INFO);
        }
        Integer id = erpReceiptOrderDetailService.saveReceiptOrderDetail(userInfo,rq);


        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,id);
    }


    @ApiOperation(value = "根据id查询销售收款详情")
    @GetMapping("getReceiptOrderDetailById")
    public ResponseResult<ErpReceiptOrderDetailDTO> getReceiptOrderDetailById(@RequestParam Integer id){
        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        ErpReceiptOrderDetailDTO dto = erpReceiptOrderDetailService.getReceiptOrderDetailById(id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dto);
    }

    @ApiOperation(value = "根据销售订单id查询销售收款情况")
    @GetMapping("getReceiptOrderDetailByOrderId")
    public ResponseResult<List<ErpReceiptOrderDetailDTO>> getReceiptOrderDetailByOrderId(@RequestParam Integer id){
        if (ObjectUtils.isEmpty(id)) {
            log.error("参数异常");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER);
        }
        List<ErpReceiptOrderDetailDTO> receiptOrderDetailByOrderId = erpReceiptOrderDetailService.getReceiptOrderDetailByOrderId(id);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,receiptOrderDetailByOrderId);
    }


}

