package com.bee.platform.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Date;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName ErpRepositoryReceiptProductOutRQ
 * @Description 功能描述
 * @Date 2019/6/5 14:17
 **/


@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("成品出库主单请求参数")
public class ErpRepositoryReceiptProductOutRQ implements Serializable {

    private static final long serialVersionUID = 2330369847424289821L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("单号")
    @NotNull(message = "单号不能为空")
    private String code;

    @ApiModelProperty("仓单类别")
    private String type;

    @ApiModelProperty("销售单公司id")
    private Integer companyId;

    @ApiModelProperty("单据日期")
    @NotNull(message = "单据日期不能为空")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date receiptDate;

    @ApiModelProperty("关联的源单据id")
    @NotNull(message = "关联的源单据id不能为空")
    private Integer relatedOrderId;

    @ApiModelProperty("关联的源单号")
//    @NotNull(message = "源单号不能为空")
    private String relatedOrder;

    @ApiModelProperty("备注")
    @Length(max=200,message = "备注不超过200字")
    private String remark;

    @ApiModelProperty("附件地址")
    private String url;


}
