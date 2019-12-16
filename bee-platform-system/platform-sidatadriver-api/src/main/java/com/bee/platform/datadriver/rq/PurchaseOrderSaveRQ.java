package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Date;

/**
 * @ClassName PurchaseOrderDetailRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/29$ 9:58$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购订单保存请求信息")
public class PurchaseOrderSaveRQ implements Serializable {

    private static final long serialVersionUID = 703208914618517275L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("合同编号")
    @NotEmpty(message = "合同编号不能为空")
    private String contractNo;

    @ApiModelProperty("合同签订日期")
    private Date contractTime;

    @ApiModelProperty("供应商id")
    @NotNull(message = "供应商id不能为空")
    private Integer supplier;

    @ApiModelProperty("供应商名称")
    @NotEmpty(message = "供应商名称不能为空")
    private String supplyName;

    @ApiModelProperty("公司id")
    @NotNull(message = "公司id不能为空")
    private Integer company;

    @ApiModelProperty("公司名称")
    @NotEmpty(message = "公司名称不能为空")
    private String companyName;

    @ApiModelProperty("采购地")
    private String place;

    @ApiModelProperty("采购方式 0现货 1期货")
    @NotNull(message = "采购方式不能为空")
    private Integer purchaseMethod;

    @ApiModelProperty("合同质量要求")
    private String requirement;

    @ApiModelProperty("备注")
    private String remark;

    @ApiModelProperty("创建人id")
    @NotNull(message = "创建人id不能为空")
    private Integer purchaseUserId;
}
