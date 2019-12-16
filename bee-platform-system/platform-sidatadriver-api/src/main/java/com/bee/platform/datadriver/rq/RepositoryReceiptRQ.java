package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * @Classname RepositoryReceiptRQ
 * @Description 采购收货单
 * @Date 2019/5/28 16:35
 * @Author xin.huang
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("采购收货单")
public class RepositoryReceiptRQ implements Serializable {

    private static final long serialVersionUID = 2330369847424289821L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("单号")
    private String code;

    @ApiModelProperty("仓单类别")
    private String type;

    @ApiModelProperty("采购公司id")
    private Integer companyId;

    @ApiModelProperty("单据日期")
    @NotEmpty(message = "单据日期不能为空")
    private String receiptDate;

    @ApiModelProperty("关联的源单据id")
    @NotNull(message = "关联的源单据id不能为空")
    private Integer relatedOrderId;

    @ApiModelProperty("关联的源单号")
    private String relatedOrder;

    @ApiModelProperty("备注")
    private String remark;

    @ApiModelProperty("附件地址")
    private String url;

    @ApiModelProperty("修改人id")
    private Integer updateUser;

}
