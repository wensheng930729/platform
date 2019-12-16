package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 产品营销方向
 * </p>
 *
 * @author chenjie123123
 * @since 2019-06-24
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("产品营销方向列表新增请求参数1")
public class ErpCrmProductMarketingAddRQ implements Serializable{

    private static final long serialVersionUID = 1438155819114555924L;

//    @ApiModelProperty("商机信息id")
//    private Integer commercialId;

    @ApiModelProperty("营销产品（码表取值）")
    private String productName;

}
