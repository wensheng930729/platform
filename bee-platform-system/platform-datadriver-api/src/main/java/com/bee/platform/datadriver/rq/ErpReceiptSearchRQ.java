package com.bee.platform.datadriver.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import java.io.Serializable;
import java.util.List;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName ErpReceiptSearchRQ
 * @Description 功能描述
 * @Date 2019/5/29 21:21
 **/

@Data
@NoArgsConstructor
@Accessors(chain=true)
@ApiModel("销售收款条件搜索的请求参数")
public class ErpReceiptSearchRQ implements Serializable {


    /**
	 * 
	 */
	private static final long serialVersionUID = -6601353767505395727L;

	@ApiModelProperty("收款单编号")
    private String code;

    @ApiModelProperty("客户名称")
    private String customerName;

//    @ApiModelProperty("公司名称")
//    private String companyName;
    @ApiModelProperty("公司名称")
    private Integer companyId;

    @ApiModelProperty("收款状态（0已保存，1已收款）")
    private Integer state;

    @ApiModelProperty("开始时间")
    private String startTime;

    @ApiModelProperty("结束时间")
    private String endTime;

    private List<Integer> list;
}
