package com.bee.platform.user.dto;

import io.swagger.annotations.ApiModel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * @ClassName AttachmentDTO
 * @Description 附件信息
 * @author qhwang
 * @version 1.0.0
 */
@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel("附件信息")
public class AttachmentDTO implements Serializable {

	private static final long serialVersionUID = -3200272247179868818L;

    /**
     * 附件名称
     */
    private String name;

    /**
     * 附件url
     */
    private String url;

}
