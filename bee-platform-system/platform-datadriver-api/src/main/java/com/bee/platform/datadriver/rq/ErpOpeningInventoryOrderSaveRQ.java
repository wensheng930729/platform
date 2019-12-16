package com.bee.platform.datadriver.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Date;

/**
 * @ClassName ErpOpeningInventoryOrderSaveRQ
 * @Description 功能描述
 * @author jie.chen
 * @Date 2019/5/30$ 16:27$
 * @version 1.0.0
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("期初库存主表保存请求信息")
public class ErpOpeningInventoryOrderSaveRQ implements Serializable {

    private static final long serialVersionUID = 2907463605080574512L;

    @ApiModelProperty("id")
    private Integer id;

    @ApiModelProperty("期初库存编号")
    @NotEmpty(message = "期初库存编号不能为空")
    private String code;

    @ApiModelProperty("期初日期")
    @NotNull(message = "期初日期不能为空")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date openingInventoryTime;

    @ApiModelProperty("公司id")
    @NotNull(message = "公司id不能为空")
    private Integer companyId;

    @ApiModelProperty("公司名称")
//    @NotEmpty(message = "公司名称不能为空")
    private String companyName;

    @ApiModelProperty("备注")
    @Length(max = 200,message = "备注信息不超过200字")
    private String remark;
}
